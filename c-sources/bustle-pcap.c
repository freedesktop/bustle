/* bustle-pcap.c: utility to log D-Bus traffic to a pcap file.
 *
 * Copyright © 2011–2018 Will Thompson <will@willthompson.co.uk>
 *
 * This program is free software; you can redistribute it and/or
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
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

#include "config.h"

#include <string.h>
#include <glib.h>
#include <glib-unix.h>
#include <glib/gprintf.h>
#include <gio/gunixinputstream.h>

#include "pcap-monitor.h"
#include "version.h"

static gboolean verbose = FALSE;
static gboolean quiet = FALSE;
static gboolean version = FALSE;
static gboolean help = FALSE;

static gboolean session_specified = FALSE;
static gboolean system_specified = FALSE;
static GBusType bus_type = G_BUS_TYPE_NONE;
static gchar *address = NULL;
static gchar **filenames = NULL;

static GOptionEntry bus_entries[] = {
    { "session", 'e', 0, G_OPTION_ARG_NONE, &session_specified,
      "Monitor session bus (default)", NULL
    },
    { "system", 'y', 0, G_OPTION_ARG_NONE, &system_specified,
      "Monitor system bus", NULL
    },
    { "address", 'a', 0, G_OPTION_ARG_STRING, &address,
      "Monitor given D-Bus address", NULL
    },
    { NULL }
};

static GOptionEntry entries[] = {
    { "verbose", 'v', 0, G_OPTION_ARG_NONE, &verbose,
      "Print brief summaries of captured messages to stdout", NULL
    },
    { "quiet", 'q', 0, G_OPTION_ARG_NONE, &quiet,
      "Don't print out instructions", NULL
    },
    { "version", 'V', 0, G_OPTION_ARG_NONE, &version,
      "Print version information and exit", NULL
    },
    { "help", 'h', 0, G_OPTION_ARG_NONE, &help,
      "Print help and exit", NULL
    },
    { G_OPTION_REMAINING, 0, 0, G_OPTION_ARG_FILENAME_ARRAY, &filenames,
      "The filename to log to", NULL
    },
    { NULL }
};

static void
parse_arguments (
    int *argc,
    char ***argv,
    gchar **filename)
{
  g_autoptr(GOptionContext) context;
  GOptionGroup *g;
  g_autofree gchar *usage;
  g_autoptr(GError) error = NULL;
  gboolean ret;
  gint exit_status = -1;
  int buses_specified;

  context = g_option_context_new ("FILENAME");
  /* We implement --help by hand because the default implementation only shows
   * the main group, and we want to show the --session/--system/--address group
   * too.
   */
  g_option_context_set_help_enabled (context, FALSE);
  g_option_context_add_main_entries (context, entries, NULL);
  g_option_context_set_summary (context, "Logs D-Bus traffic to FILENAME in a format suitable for bustle");

  g = g_option_group_new ("bus",
                          "Bus Options:",
                          "Options specifying the bus to monitor",
                          NULL,
                          NULL);
  g_option_group_add_entries (g, bus_entries);
  g_option_context_add_group (context, g);

  ret = g_option_context_parse (context, argc, argv, &error);
  usage = g_option_context_get_help (context, FALSE, NULL);

  if (!ret)
    {
      fprintf (stderr, "%s\n", error->message);
      fprintf (stderr, "%s", usage);

      exit_status = 2;
      goto out;
    }

  buses_specified = !!session_specified + !!system_specified + !!address;

  if (help)
    {
      g_print ("%s", usage);
      exit_status = 0;
      goto out;
    }
  else if (version)
    {
      fprintf (stdout, "bustle-pcap " BUSTLE_VERSION "\n\n");
      fprintf (stdout, "Copyright © 2011–2018 Will Thompson <will@willthompson.co.uk>\n");
      fprintf (stdout, "This is free software; see the source for copying conditions.  There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n\n");
      fprintf (stdout, "Written by Will Thompson <will@willthompson.co.uk>\n");

      exit_status = 0;
      goto out;
    }
  else if (buses_specified > 1)
    {
      fprintf (stderr, "You may only specify one of --session, --system and --address\n");
      fprintf (stderr, "%s", usage);

      exit_status = 2;
      goto out;
    }
  else if (address != NULL)
    {
      bus_type = G_BUS_TYPE_NONE;
      /* and the caller will use the global variable for the address */
    }
  else if (system_specified)
    {
      bus_type = G_BUS_TYPE_SYSTEM;
    }
  else
    {
      bus_type = G_BUS_TYPE_SESSION;
    }

  if (filenames == NULL ||
      filenames[0] == NULL ||
      filenames[1] != NULL)
    {
      fprintf (stderr, "You must specify exactly one output filename\n");
      fprintf (stderr, "%s", usage);

      exit_status = 2;
      goto out;
    }

  *filename = g_strdup (filenames[0]);

out:
  g_strfreev (filenames);

  if (exit_status > -1)
    exit (exit_status);
}

static void
message_logged_cb (
    BustlePcapMonitor *pcap,
    glong sec,
    glong usec,
    guint8 *data,
    guint len,
    gpointer user_data)
{
  g_autoptr(GError) error = NULL;
  g_autoptr(GDBusMessage) message = g_dbus_message_new_from_blob (
      data, len, G_DBUS_CAPABILITY_FLAGS_UNIX_FD_PASSING, &error);

  if (message == NULL)
    g_warning ("%s", error->message);
  else
    g_print ("%s -> %s: %d %s\n",
        g_dbus_message_get_sender (message),
        g_dbus_message_get_destination (message),
        g_dbus_message_get_message_type (message),
        g_dbus_message_get_member (message));
}

static void
stopped_cb (
    BustlePcapMonitor *pcap,
    guint domain,
    gint code,
    const gchar *message,
    gpointer user_data)
{
  GMainLoop *loop = user_data;

  if (!(domain == G_IO_ERROR && code == G_IO_ERROR_CANCELLED))
    g_printerr ("Error while monitoring: %s\n", message);

  g_main_loop_quit (loop);
}

static gboolean
sigint_cb (gpointer user_data)
{
  BustlePcapMonitor *pcap = user_data;

  bustle_pcap_monitor_stop (pcap);

  return G_SOURCE_CONTINUE;
}

int
main (
    int argc,
    char **argv)
{
  GMainLoop *loop;
  gchar *filename;
  g_autoptr(GError) error = NULL;
  BustlePcapMonitor *pcap;

  parse_arguments (&argc, &argv, &filename);

  pcap = bustle_pcap_monitor_new (bus_type, address, filename, &error);
  if (pcap == NULL)
    {
      g_printerr ("Failed to start monitor: %s\n",
                  error->message);
      exit (1);
    }

  if (verbose)
    g_signal_connect (pcap, "message-logged",
        G_CALLBACK (message_logged_cb), NULL);

  loop = g_main_loop_new (NULL, FALSE);
  g_signal_connect (pcap, "stopped", G_CALLBACK (stopped_cb), loop);

  if (!quiet)
    g_printf ("Logging D-Bus traffic to '%s'...\n", filename);

  g_unix_signal_add (SIGINT, sigint_cb, pcap);

  if (!quiet)
    g_printf ("Hit Control-C to stop logging.\n");

  g_main_loop_run (loop);
  g_main_loop_unref (loop);

  bustle_pcap_monitor_stop (pcap);
  g_object_unref (pcap);
  g_free (filename);

  return 0;
}
