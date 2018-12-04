/*
 * pcap-monitor.c - monitors a bus and dumps messages to a pcap file
 * Copyright © 2011–2012 Collabora Ltd.
 * Copyright © 2018      Will Thompson
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include "config.h"
#include "pcap-monitor.h"

#include <errno.h>
#include <signal.h>
#include <string.h>
#include <pcap/pcap.h>
#include <gio/gunixinputstream.h>

/* Prefix of name claimed by the connection that collects name owners. */
const char *BUSTLE_MONITOR_NAME_PREFIX = "org.freedesktop.Bustle.Monitor.";

/*
 * Transitions:
 *
 * NEW      --[ initable_init() errors ]--> STOPPED
 * NEW      --[ initable_init()        ]--> STARTING
 *
 * STARTING --[ start_pcap()           ]--> RUNNING
 * STARTING --[ error                  ]--> STOPPING
 *
 * RUNNING  --[ user request/error     ]--> STOPPING
 *
 * STOPPING --[ all finished           ]--> STOPPED
 */
typedef enum {
    /* Nothing's happened yet */
    STATE_NEW,

    /* We're waiting to read the pcap header from the subprocess */
    STATE_STARTING,

    /* We've read the pcap header, messages are flowing freely */
    STATE_RUNNING,

    /* Error or user request is causing us to stop; we're waiting to consume
     * everything from the pipe and for the dbus-monitor subprocess to exit.
     */
    STATE_STOPPING,

    /* We've stopped, whether by user decision or because of an error.
     * Everything has been torn down (except possibly a root-owned subprocess),
     * the output file has been fully written and closed, and no more signals
     * will be emitted.
     */
    STATE_STOPPED,
} BustlePcapMonitorState;

static const gchar * const STATES[] = {
    "NEW",
    "STARTING",
    "RUNNING",
    "STOPPING",
    "STOPPED",
};

typedef struct _BustlePcapMonitor {
    GObject parent;

    GBusType bus_type;
    gchar *address;
    BustlePcapMonitorState state;
    GCancellable *cancellable;
    guint cancellable_cancelled_id;

    /* input */
    GSubprocess *dbus_monitor;
    GSource *dbus_monitor_source;
    pcap_t *pcap_in;

    /* output */
    gchar *filename;
    pcap_t *pcap_out;
    pcap_dumper_t *dumper;

    /* errors */
    GError *pcap_error;
    GError *subprocess_error;
    guint await_both_errors_id;
} BustlePcapMonitor;

enum {
    PROP_BUS_TYPE = 1,
    PROP_ADDRESS,
    PROP_FILENAME,
};

enum {
    SIG_MESSAGE_LOGGED,
    SIG_STOPPED,
    N_SIGNALS
};

static guint signals[N_SIGNALS];

static void initable_iface_init (
    gpointer g_class,
    gpointer unused);

G_DEFINE_TYPE_WITH_CODE (BustlePcapMonitor, bustle_pcap_monitor, G_TYPE_OBJECT,
    G_IMPLEMENT_INTERFACE (G_TYPE_INITABLE, initable_iface_init);
    )

static void
bustle_pcap_monitor_init (BustlePcapMonitor *self)
{
  self->bus_type = G_BUS_TYPE_SESSION;
  self->state = STATE_NEW;
  self->cancellable = g_cancellable_new ();
}

static void
bustle_pcap_monitor_get_property (
    GObject *object,
    guint property_id,
    GValue *value,
    GParamSpec *pspec)
{
  BustlePcapMonitor *self = BUSTLE_PCAP_MONITOR (object);

  switch (property_id)
    {
      case PROP_BUS_TYPE:
        g_value_set_enum (value, self->bus_type);
        break;
      case PROP_ADDRESS:
        g_value_set_string (value, self->address);
        break;
      case PROP_FILENAME:
        g_value_set_string (value, self->filename);
        break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
    }
}

static void
bustle_pcap_monitor_set_property (
    GObject *object,
    guint property_id,
    const GValue *value,
    GParamSpec *pspec)
{
  BustlePcapMonitor *self = BUSTLE_PCAP_MONITOR (object);

  switch (property_id)
    {
      case PROP_BUS_TYPE:
        self->bus_type = g_value_get_enum (value);
        break;
      case PROP_ADDRESS:
        self->address = g_value_dup_string (value);
        break;
      case PROP_FILENAME:
        self->filename = g_value_dup_string (value);
        break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
    }
}

static void
close_dump (BustlePcapMonitor *self)
{
  if (self->dumper != NULL)
    pcap_dump_flush (self->dumper);

  g_clear_pointer (&self->dumper, pcap_dump_close);
  g_clear_pointer (&self->pcap_out, pcap_close);
}

static void
bustle_pcap_monitor_dispose (GObject *object)
{
  BustlePcapMonitor *self = BUSTLE_PCAP_MONITOR (object);
  GObjectClass *parent_class = bustle_pcap_monitor_parent_class;

  if (self->cancellable_cancelled_id != 0)
    {
      g_assert (self->cancellable != NULL);
      g_cancellable_disconnect (self->cancellable, self->cancellable_cancelled_id);
      self->cancellable_cancelled_id = 0;
    }

  g_clear_object (&self->cancellable);
  g_clear_pointer (&self->dbus_monitor_source, g_source_destroy);
  g_clear_pointer (&self->pcap_in, pcap_close);
  g_clear_object (&self->dbus_monitor);

  close_dump (self);

  if (parent_class->dispose != NULL)
    parent_class->dispose (object);
}

static void
bustle_pcap_monitor_finalize (GObject *object)
{
  BustlePcapMonitor *self = BUSTLE_PCAP_MONITOR (object);
  GObjectClass *parent_class = bustle_pcap_monitor_parent_class;

  g_clear_pointer (&self->address, g_free);
  g_clear_pointer (&self->filename, g_free);
  g_clear_error (&self->pcap_error);
  g_clear_error (&self->subprocess_error);

  if (parent_class->finalize != NULL)
    parent_class->finalize (object);
}

static void
bustle_pcap_monitor_class_init (BustlePcapMonitorClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
  GParamSpec *param_spec;

  object_class->get_property = bustle_pcap_monitor_get_property;
  object_class->set_property = bustle_pcap_monitor_set_property;
  object_class->dispose = bustle_pcap_monitor_dispose;
  object_class->finalize = bustle_pcap_monitor_finalize;

#define THRICE(x) x, x, x

  param_spec = g_param_spec_enum (
      "bus-type", "Bus type",
      "Which standard bus to monitor. If NONE, :address must be non-NULL.",
      G_TYPE_BUS_TYPE, G_BUS_TYPE_NONE,
      G_PARAM_CONSTRUCT_ONLY | G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS);
  g_object_class_install_property (object_class, PROP_BUS_TYPE, param_spec);

  param_spec = g_param_spec_string (
      "address", "Address",
      "Address of bus to monitor. If non-NULL, :bus-type must be G_BUS_TYPE_NONE",
      NULL,
      G_PARAM_CONSTRUCT_ONLY | G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS);
  g_object_class_install_property (object_class, PROP_ADDRESS, param_spec);

  param_spec = g_param_spec_string (THRICE ("filename"), NULL,
      G_PARAM_CONSTRUCT_ONLY | G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS);
  g_object_class_install_property (object_class, PROP_FILENAME, param_spec);

  /**
   * BustlePcapMonitor::message-logged:
   * @self: the monitor.
   * @sec: seconds since 1970.
   * @usec: microseconds! (These are not combined into a single %gint64 because
   *  gtk2hs as of 2018-01-08 crashes when it encounters %G_TYPE_UINT64 in a
   *  #GValue.)
   * @blob: an array of bytes containing the serialized message.
   * @length: the size in bytes of @blob.
   */
  signals[SIG_MESSAGE_LOGGED] = g_signal_new ("message-logged",
      BUSTLE_TYPE_PCAP_MONITOR, G_SIGNAL_RUN_FIRST,
      0, NULL, NULL,
      NULL, G_TYPE_NONE, 4,
      G_TYPE_LONG,
      G_TYPE_LONG,
      G_TYPE_POINTER,
      G_TYPE_UINT);

  /**
   * BustlePcapMonitor::stopped:
   * @self: the monitor
   * @domain: domain of a #GError (as G_TYPE_UINT because there is no
   *          G_TYPE_UINT32)
   * @code: code of a #GError
   * @message: message of a #GError
   *
   * Emitted once when monitoring stops, whether triggered (asynchronously) by
   * calling bustle_pcap_monitor_stop(), in which case @domain will be
   * %G_IO_ERROR and @code will be %G_IO_ERROR_CANCELLED, or because an error
   * occurs.
   */
  signals[SIG_STOPPED] = g_signal_new ("stopped",
      BUSTLE_TYPE_PCAP_MONITOR, G_SIGNAL_RUN_FIRST,
      0, NULL, NULL,
      NULL, G_TYPE_NONE, 3,
      G_TYPE_UINT,
      G_TYPE_INT,
      G_TYPE_STRING);
}

static void
handle_error (BustlePcapMonitor *self)
{
  g_autoptr(GError) error = NULL;

  g_return_if_fail (self->pcap_error != NULL ||
                    self->subprocess_error != NULL);

  if (self->pcap_error != NULL)
    g_debug ("%s: pcap_error: %s", G_STRFUNC, self->pcap_error->message);

  if (self->subprocess_error != NULL)
    g_debug ("%s: subprocess_error: %s", G_STRFUNC,
             self->subprocess_error->message);

  if (self->state == STATE_STOPPED)
    {
      g_debug ("%s: already stopped", G_STRFUNC);
      return;
    }

  /* Check for pkexec errors. Signal these in preference to all others. */
  if (self->subprocess_error != NULL &&
      self->bus_type == G_BUS_TYPE_SYSTEM)
    {
      if (g_error_matches (self->subprocess_error, G_SPAWN_EXIT_ERROR, 126))
        {
          /* dialog dismissed */
          g_set_error (&error, G_IO_ERROR, G_IO_ERROR_CANCELLED,
                       "User dismissed polkit authorization dialog");
        }
      else if (g_error_matches (self->subprocess_error, G_SPAWN_EXIT_ERROR, 127))
        {
          /* not authorized, authorization couldn't be obtained through
           * authentication, or an self->subprocess_error occurred */
          g_set_error (&error, G_IO_ERROR, G_IO_ERROR_PERMISSION_DENIED,
                       "Not authorized to monitor system bus");
        }
    }

  if (g_error_matches (self->subprocess_error, G_SPAWN_EXIT_ERROR, 0))
    {
      /* I believe clean exit only happens if the bus is shut down. This might
       * happen if you're using Bustle to monitor a test suite, or perhaps a
       * user session that you log out of. Let's consider this to be
       * cancellation.
       */
      g_set_error_literal (&error, G_IO_ERROR, G_IO_ERROR_CANCELLED,
                           self->subprocess_error->message);
    }

  if (error == NULL)
    {
      /* If no pkexec errors, prefer potentially more informative errors from
       * libpcap, including the wonderful snaplen bug.
       */
      if (self->pcap_error != NULL)
        {
          error = g_steal_pointer (&self->pcap_error);
        }
      /* Otherwise, the "subprocess didn't work" error will have to do. */
      else
        {
          error = g_steal_pointer (&self->subprocess_error);

          if (self->state == STATE_STARTING)
            g_prefix_error (&error, "Failed to start dbus-monitor: ");
        }
    }

  self->state = STATE_STOPPED;
  close_dump (self);

  g_debug ("%s: emitting ::stopped(%s, %d, %s)", G_STRFUNC,
           g_quark_to_string (error->domain), error->code, error->message);
  g_signal_emit (self, signals[SIG_STOPPED], 0,
                 (guint) error->domain, error->code, error->message);

  g_clear_handle_id (&self->await_both_errors_id, g_source_remove);
}

static gboolean
await_both_errors_cb (gpointer data)
{
  BustlePcapMonitor *self = BUSTLE_PCAP_MONITOR (data);

  handle_error (self);

  self->await_both_errors_id = 0;
  return G_SOURCE_REMOVE;
}

/* Wait for the dbus-monitor subprocess to have exited, and for the pcap reader
 * to have finished. We expect the reader to finish promptly when the
 * subprocess does, but the subprocess may not die until it tries to read to a
 * closed pipe (if the user stops the recording). So we wait a couple of
 * seconds before pressing on.
 */
static void
await_both_errors (BustlePcapMonitor *self)
{
  if (self->state == STATE_STOPPED)
    return;
  else if (self->subprocess_error != NULL && self->pcap_error != NULL)
    handle_error (self);
  else if (self->await_both_errors_id == 0)
    self->await_both_errors_id =
      g_timeout_add_seconds_full (G_PRIORITY_DEFAULT, 2, await_both_errors_cb,
                                  g_object_ref (self), g_object_unref);
}

static gboolean
list_all_names (
    GDBusProxy *bus,
    GError **error)
{
  g_autoptr(GVariant) ret = NULL;
  gchar **names;  /* borrowed from 'ret' */

  g_return_val_if_fail (G_IS_DBUS_PROXY (bus), FALSE);

  ret = g_dbus_proxy_call_sync (bus, "ListNames", NULL,
      G_DBUS_CALL_FLAGS_NONE, -1, NULL, error);
  if (ret == NULL)
    {
      g_prefix_error (error, "Couldn't ListNames: ");
      return FALSE;
    }

  for (g_variant_get_child (ret, 0, "^a&s", &names);
       *names != NULL;
       names++)
    {
      gchar *name = *names;

      if (!g_dbus_is_unique_name (name) &&
          strcmp (name, "org.freedesktop.DBus") != 0)
        {
          g_autoptr(GVariant) owner =
            g_dbus_proxy_call_sync (bus, "GetNameOwner",
                                    g_variant_new ("(s)", name),
                                    G_DBUS_CALL_FLAGS_NONE, -1, NULL, NULL);
          /* Ignore returned value or error. These are just used by the UI to
           * fill in the initial owners of each well-known name. If we get an
           * error here, the owner disappeared between ListNames() and here;
           * but that means we'll have seen a NameOwnerChanged from which the
           * UI can (in theory) infer who the owner used to be.
           *
           * We cannot use G_DBUS_MESSAGE_FLAGS_NO_REPLY_EXPECTED because we
           * do want the reply to be sent to us.
           */
        }
    }

  return TRUE;
}

static GDBusConnection *
get_connection (
    BustlePcapMonitor *self,
    GCancellable *cancellable,
    GError **error)
{
  g_autofree gchar *address_to_free = NULL;
  const gchar *address = self->address;

  if (self->address != NULL)
    {
      address = self->address;
    }
  else
    {
      address_to_free = g_dbus_address_get_for_bus_sync (self->bus_type,
                                                         cancellable, error);
      if (address_to_free == NULL)
        {
          g_prefix_error (error, "Couldn't get bus address: ");
          return FALSE;
        }

      address = address_to_free;
    }

  return g_dbus_connection_new_for_address_sync (
      address,
      G_DBUS_CONNECTION_FLAGS_AUTHENTICATION_CLIENT |
      G_DBUS_CONNECTION_FLAGS_MESSAGE_BUS_CONNECTION,
      NULL, /* auth observer */
      cancellable,
      error);
}

static void
dump_names_thread_func (
    GTask *task,
    gpointer source_object,
    gpointer task_data,
    GCancellable *cancellable)
{
  BustlePcapMonitor *self = BUSTLE_PCAP_MONITOR (source_object);
  g_autoptr(GDBusConnection) connection = NULL;
  g_autoptr(GDBusProxy) bus = NULL;
  g_autoptr(GError) error = NULL;

  connection = get_connection (self, cancellable, &error);
  if (connection != NULL)
    {
      const gchar *unique_name = g_dbus_connection_get_unique_name (connection);

      if (unique_name != NULL)
        {
          g_autofree gchar *mangled = g_strdup (unique_name);
          g_autofree gchar *well_known_name =
            g_strconcat (BUSTLE_MONITOR_NAME_PREFIX,
                         /* ":3.14" -> "_3_14", a legal bus name component */
                         g_strcanon (mangled, "0123456789", '_'),
                         NULL);

          g_debug ("%s: attempting to own %s", G_STRFUNC, well_known_name);
          /* Ignore returned ID: we'll own it until we disconnect.
           *
           * Also ignore callbacks: we don't need to be notified, since this
           * name is just for the benefit of the viewer. On the system bus we
           * won't be able to own this name, but if we're smart we can teach
           * the viewer that merely requesting the name is enough to hide this
           * connection.
           */
          g_bus_own_name_on_connection (connection,
                                        well_known_name,
                                        G_BUS_NAME_OWNER_FLAGS_NONE,
                                        NULL /* acquired */,
                                        NULL /* lost */,
                                        NULL /* user_data */,
                                        NULL /* free_func */);
        }

      bus = g_dbus_proxy_new_sync (connection,
                                   G_DBUS_PROXY_FLAGS_DO_NOT_LOAD_PROPERTIES |
                                   G_DBUS_PROXY_FLAGS_DO_NOT_CONNECT_SIGNALS |
                                   G_DBUS_PROXY_FLAGS_DO_NOT_AUTO_START,
                                   NULL,
                                   "org.freedesktop.DBus",
                                   "/org/freedesktop/DBus",
                                   "org.freedesktop.DBus",
                                   cancellable,
                                   &error);
    }

  if (bus != NULL && list_all_names (bus, &error))
    g_task_return_boolean (task, TRUE);
  else
    g_task_return_error (task, g_steal_pointer (&error));

  g_assert (error == NULL);
  if (connection != NULL
      && !g_dbus_connection_close_sync (connection, cancellable, &error))
    g_warning ("%s: %s", G_STRFUNC, error->message);
}

static void
dump_names_cb (
    GObject *source_object,
    GAsyncResult *result,
    gpointer user_data)
{
  g_autoptr(GError) error = NULL;

  if (!g_task_propagate_boolean (G_TASK (result), &error))
    g_warning ("Failed to dump names: %s", error->message);
}

static void
dump_names_async (
    BustlePcapMonitor *self)
{
  g_autoptr(GTask) task = g_task_new (self, self->cancellable, dump_names_cb, NULL);

  g_task_run_in_thread (task, dump_names_thread_func);
}

static gboolean
start_pcap (
    BustlePcapMonitor *self,
    GError **error)
{
  GInputStream *stdout_pipe = NULL;
  gint stdout_fd = -1;
  FILE *dbus_monitor_filep = NULL;
  char errbuf[PCAP_ERRBUF_SIZE] = {0};

  stdout_pipe = g_subprocess_get_stdout_pipe (self->dbus_monitor);
  g_return_val_if_fail (stdout_pipe != NULL, FALSE);

  stdout_fd = g_unix_input_stream_get_fd (G_UNIX_INPUT_STREAM (stdout_pipe));
  g_return_val_if_fail (stdout_fd >= 0, FALSE);

  dbus_monitor_filep = fdopen (stdout_fd, "r");
  if (dbus_monitor_filep == NULL)
    {
      g_set_error (error, G_IO_ERROR, g_io_error_from_errno (errno), "fdopen");
      return FALSE;
    }
  /* fd is owned by the FILE * now */
  g_unix_input_stream_set_close_fd (G_UNIX_INPUT_STREAM (stdout_pipe), FALSE);

  /* This reads the 4-byte pcap header from the pipe, in a single blocking
   * fread(). It's safe to do this on the main thread, since we know the pipe
   * is readable. On short read, pcap_fopen_offline() fails immediately.
   */
  self->pcap_in = pcap_fopen_offline (dbus_monitor_filep, errbuf);
  if (self->pcap_in == NULL)
    {
      g_set_error (error, G_IO_ERROR, G_IO_ERROR_FAILED,
                   "Couldn't read messages from dbus-monitor: %s",
                   errbuf);

      /* Cause dbus-monitor to exit next time it tries to write a message */
      g_clear_pointer (&dbus_monitor_filep, fclose);

      /* And try to terminate it immediately. When spawning via pkexec we may
       * not be able to kill it.
       */
      g_subprocess_force_exit (self->dbus_monitor);

      return FALSE;
    }

  /* pcap_close() will call fclose() on the FILE * passed to
   * pcap_fopen_offline() */
  dump_names_async (self);
  self->state = STATE_RUNNING;
  return TRUE;
}

static gboolean
read_one (
    BustlePcapMonitor *self,
    GError **error)
{
  struct pcap_pkthdr *hdr;
  const guchar *blob;
  int ret;

  ret = pcap_next_ex (self->pcap_in, &hdr, &blob);
  switch (ret)
    {
      case 1:
        g_signal_emit (self, signals[SIG_MESSAGE_LOGGED], 0,
            hdr->ts.tv_sec, hdr->ts.tv_usec, blob, hdr->caplen);

        /* cast necessary because pcap_dump has a type matching the callback
         * argument to pcap_loop()
         * TODO don't block
         */
        pcap_dump ((u_char *) self->dumper, hdr, blob);
        return TRUE;

      case -2:
        /* EOF; shouldn't happen since we waited for the FD to be readable */
        g_set_error (error, G_IO_ERROR, G_IO_ERROR_CONNECTION_CLOSED,
            "EOF when reading from dbus-monitor");
        return FALSE;

      default:
        g_set_error (error, G_IO_ERROR, G_IO_ERROR_FAILED,
            "Error %i reading dbus-monitor stream: %s",
            ret, pcap_geterr (self->pcap_in));
        return FALSE;
    }
}




static gboolean
dbus_monitor_readable (
    GObject *pollable_input_stream,
    gpointer user_data)
{
  BustlePcapMonitor *self = BUSTLE_PCAP_MONITOR (user_data);
  gboolean (*read_func) (BustlePcapMonitor *, GError **);

  g_return_val_if_fail (self->pcap_error == NULL, FALSE);

  if (g_cancellable_set_error_if_cancelled (self->cancellable, &self->pcap_error))
    {
      await_both_errors (self);
      return FALSE;
    }

  switch (self->state)
    {
    case STATE_STARTING:
      read_func = start_pcap;
      break;

    case STATE_RUNNING:
    case STATE_STOPPING: /* may have a few last messages to read */
      read_func = read_one;
      break;

    default:
      g_critical ("%s in unexpected state %d (%s)",
                  G_STRFUNC, self->state, STATES[self->state]);
      return FALSE;
    }

  if (!read_func (self, &self->pcap_error))
    {
      await_both_errors (self);
      return FALSE;
    }

  return TRUE;
}

static void
wait_check_cb (
    GObject *source,
    GAsyncResult *result,
    gpointer user_data)
{
  g_autoptr(BustlePcapMonitor) self = BUSTLE_PCAP_MONITOR (user_data);
  GSubprocess *dbus_monitor = G_SUBPROCESS (source);

  g_return_if_fail (self->subprocess_error == NULL);

  if (g_subprocess_wait_check_finish (dbus_monitor, result,
                                      &self->subprocess_error))
    {
      g_set_error (&self->subprocess_error,
                   G_SPAWN_EXIT_ERROR, 0,
                   "dbus-monitor exited cleanly");
    }

  /* cases:
   * - G_SPAWN_ERROR / G_SPAWN_ERROR_FAILED / "Child process killed by signal N":
   *   dbus-monitor was killed, possibly by us calling
   *   g_subprocess_force_exit(), though this doesn't work for pkexec'd
   *   dbus-monitor
   * - G_SPAWN_EXIT_ERROR:
   *   - 0: bus itself went away (assuming pkexec/flatpak-spawn propagate
   *        errors correctly)
   *   - 1: anything else went wrong in dbus-monitor, including invalid
   *        arguments and broken pipe (when we close the read end)
   *   - 126: User dismissed polkit authentication dialog
   *   - 127: polkit auth failed
   *   - 128 + N: killed by signal N, propagated by flatpak-spawn --host
   *
   * We just need to deal with 0, 126, 127 specially.
   */
  await_both_errors (self);
}

static void
cancellable_cancelled_cb (GCancellable *cancellable,
                          gpointer      user_data)
{
  BustlePcapMonitor *self = BUSTLE_PCAP_MONITOR (user_data);

  /* Closes the stream; should cause dbus-monitor to quit in due course when it
   * tries to write to the other end of the pipe.
   */
  g_clear_pointer (&self->pcap_in, pcap_close);

  if (self->dbus_monitor != NULL)
    {
      /* Try to make it stop sooner; this has no effect on a privileged
       * dbus-monitor.
       */
      g_subprocess_force_exit (self->dbus_monitor);
    }
}

static const char **
build_argv (BustlePcapMonitor *self,
            GError **error)
{
  g_autoptr(GPtrArray) dbus_monitor_argv = g_ptr_array_sized_new (8);
  gboolean in_flatpak = g_file_test ("/.flatpak-info", G_FILE_TEST_EXISTS);

  if (in_flatpak)
    {
      g_ptr_array_add (dbus_monitor_argv, "flatpak-spawn");
      g_ptr_array_add (dbus_monitor_argv, "--host");
    }

  if (self->bus_type == G_BUS_TYPE_SYSTEM)
    g_ptr_array_add (dbus_monitor_argv, "pkexec");

  g_ptr_array_add (dbus_monitor_argv, "dbus-monitor");
  g_ptr_array_add (dbus_monitor_argv, "--pcap");

  switch (self->bus_type)
    {
      case G_BUS_TYPE_SESSION:
        g_return_val_if_fail (self->address == NULL, FALSE);
        g_ptr_array_add (dbus_monitor_argv, "--session");
        break;

      case G_BUS_TYPE_SYSTEM:
        g_return_val_if_fail (self->address == NULL, FALSE);
        g_ptr_array_add (dbus_monitor_argv, "--system");
        break;

      case G_BUS_TYPE_NONE:
        g_return_val_if_fail (self->address != NULL, FALSE);
        g_ptr_array_add (dbus_monitor_argv, "--address");
        g_ptr_array_add (dbus_monitor_argv, self->address);
        break;

      default:
        g_set_error (error, G_IO_ERROR, G_IO_ERROR_NOT_SUPPORTED,
            "Can only log the session bus, system bus, or a given address");
        return NULL;
    }

  g_ptr_array_add (dbus_monitor_argv, NULL);
  return (const char **) g_ptr_array_free (g_steal_pointer (&dbus_monitor_argv), FALSE);
}

static GSubprocess *
spawn_monitor (BustlePcapMonitor *self,
               const char *const *argv,
               GError **error)
{
  g_autoptr(GSubprocessLauncher) launcher =
    g_subprocess_launcher_new (G_SUBPROCESS_FLAGS_STDOUT_PIPE);

  return g_subprocess_launcher_spawnv (launcher, argv, error);
}

static gboolean
initable_init (
    GInitable *initable,
    GCancellable *cancellable,
    GError **error)
{
  BustlePcapMonitor *self = BUSTLE_PCAP_MONITOR (initable);
  g_autofree const char **argv = NULL;
  GInputStream *stdout_pipe = NULL;

  argv = build_argv (self, error);
  if (NULL == argv)
    return FALSE;

  if (self->filename == NULL)
    {
      g_set_error (error, G_IO_ERROR, G_IO_ERROR_INVALID_ARGUMENT,
          "You must specify a filename");
      return FALSE;
    }

  self->cancellable_cancelled_id =
    g_cancellable_connect (self->cancellable,
                           G_CALLBACK (cancellable_cancelled_cb),
                           self, NULL);

  self->pcap_out = pcap_open_dead (DLT_DBUS, 1 << 27);
  if (self->pcap_out == NULL)
    {
      g_set_error (error, G_IO_ERROR, G_IO_ERROR_FAILED,
          "pcap_open_dead failed. wtf");
      return FALSE;
    }

  self->dumper = pcap_dump_open (self->pcap_out, self->filename);
  if (self->dumper == NULL)
    {
      g_set_error (error, G_IO_ERROR, G_IO_ERROR_FAILED,
          "Couldn't open target file %s", pcap_geterr (self->pcap_out));
      return FALSE;
    }

  self->dbus_monitor = spawn_monitor (self, (const char * const *) argv, error);
  if (self->dbus_monitor == NULL)
    return FALSE;

  stdout_pipe = g_subprocess_get_stdout_pipe (self->dbus_monitor);
  g_return_val_if_fail (stdout_pipe != NULL, FALSE);
  g_return_val_if_fail (G_IS_POLLABLE_INPUT_STREAM (stdout_pipe), FALSE);
  g_return_val_if_fail (G_IS_UNIX_INPUT_STREAM (stdout_pipe), FALSE);

  self->dbus_monitor_source = g_pollable_input_stream_create_source (
      G_POLLABLE_INPUT_STREAM (stdout_pipe), self->cancellable);
  g_source_set_callback (self->dbus_monitor_source,
      (GSourceFunc) dbus_monitor_readable, self, NULL);
  g_source_attach (self->dbus_monitor_source, NULL);

  g_subprocess_wait_check_async (
      self->dbus_monitor,
      self->cancellable,
      wait_check_cb, g_object_ref (self));

  self->state = STATE_STARTING;
  return TRUE;
}

/* FIXME: instead of GInitable + syncronous stop, have
 * bustle_pcap_monitor_record_{async,finish} */
void
bustle_pcap_monitor_stop (
    BustlePcapMonitor *self)
{
  if (self->state == STATE_STOPPED ||
      self->state == STATE_STOPPING ||
      self->state == STATE_NEW)
    {
      g_debug ("%s: already in state %s", G_STRFUNC, STATES[self->state]);
      return;
    }

  self->state = STATE_STOPPING;
  g_cancellable_cancel (self->cancellable);
}

static void
initable_iface_init (
    gpointer g_class,
    gpointer unused)
{
  GInitableIface *iface = g_class;

  iface->init = initable_init;
}

BustlePcapMonitor *
bustle_pcap_monitor_new (
    GBusType bus_type,
    const gchar *address,
    const gchar *filename,
    GError **error)
{
  return g_initable_new (
      BUSTLE_TYPE_PCAP_MONITOR, NULL, error,
      "bus-type", bus_type,
      "address", address,
      "filename", filename,
      NULL);
}
