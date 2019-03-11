#include "bustle-pcap-reader.h"

#include <pcap/pcap.h>

struct _BustlePcapReader
{
  GObject parent_instance;

  gchar *path;
  pcap_t *savefile;
};

static void bustle_pcap_reader_initable_iface_init (GInitableIface *iface,
                                                    gpointer        user_data);

G_DEFINE_TYPE_WITH_CODE (BustlePcapReader, bustle_pcap_reader, G_TYPE_OBJECT,
                         G_IMPLEMENT_INTERFACE (G_TYPE_INITABLE, bustle_pcap_reader_initable_iface_init))

typedef enum {
  PROP_PATH = 1,
  N_PROPS
} BustlePcapReaderProperty;

static GParamSpec *properties [N_PROPS];

BustlePcapReader *bustle_pcap_reader_new (const gchar *path,
                                          GError **error)
{
  return g_initable_new (BUSTLE_TYPE_PCAP_READER,
                         NULL,
                         error,
                         "path", path,
                         NULL);
}

static void
bustle_pcap_reader_finalize (GObject *object)
{
  BustlePcapReader *self = (BustlePcapReader *)object;

  g_clear_pointer (&self->path, g_free);
  g_clear_pointer (&self->savefile, pcap_close);

  G_OBJECT_CLASS (bustle_pcap_reader_parent_class)->finalize (object);
}

static void
bustle_pcap_reader_get_property (GObject    *object,
                                 guint       prop_id,
                                 GValue     *value,
                                 GParamSpec *pspec)
{
  BustlePcapReader *self = BUSTLE_PCAP_READER (object);

  switch ((BustlePcapReaderProperty) prop_id)
    {
    case PROP_PATH:
      g_value_set_string (value, self->path);
      break;
    case N_PROPS:
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
    }
}

static void
bustle_pcap_reader_set_property (GObject      *object,
                                 guint         prop_id,
                                 const GValue *value,
                                 GParamSpec   *pspec)
{
  BustlePcapReader *self = BUSTLE_PCAP_READER (object);

  switch ((BustlePcapReaderProperty) prop_id)
    {
    case PROP_PATH:
      g_assert (self->path == NULL);
      self->path = g_value_dup_string (value);
      break;

    case N_PROPS:
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
    }
}

static void
bustle_pcap_reader_class_init (BustlePcapReaderClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->finalize = bustle_pcap_reader_finalize;
  object_class->get_property = bustle_pcap_reader_get_property;
  object_class->set_property = bustle_pcap_reader_set_property;

  properties [PROP_PATH] =
    g_param_spec_string ("path",
                         "Path",
                         "Path",
                         NULL,
                         (G_PARAM_READWRITE |
                          G_PARAM_CONSTRUCT_ONLY |
                          G_PARAM_STATIC_STRINGS));
  g_object_class_install_property (object_class, PROP_PATH,
                                   properties [PROP_PATH]);
}

static void
bustle_pcap_reader_init (BustlePcapReader *self)
{
}

static gboolean
bustle_pcap_reader_initable_init (GInitable     *initable,
                                  GCancellable  *cancellable,
                                  GError       **error)
{
  BustlePcapReader *self = BUSTLE_PCAP_READER (initable);
  char errbuf[PCAP_ERRBUF_SIZE];

  g_return_val_if_fail (self->path != NULL, FALSE);

  self->savefile = pcap_open_offline (self->path, errbuf);
  if (self->savefile == NULL)
    {
      g_set_error (error, G_IO_ERROR, G_IO_ERROR_FAILED,
                   "Could not open %s: %s", self->path, errbuf);
      return FALSE;
    }

  int dlt = pcap_datalink (self->savefile);
  if (dlt != DLT_DBUS)
    {
      g_set_error (error, G_IO_ERROR, G_IO_ERROR_INVALID_DATA,
                   "%s has unexpected link type %s (%d)",
                   self->path,
                   pcap_datalink_val_to_name (dlt),
                   dlt);
      return FALSE;
    }

  return TRUE;
}

static void
bustle_pcap_reader_initable_iface_init (GInitableIface *iface,
                                        gpointer        user_data)
{
  iface->init = bustle_pcap_reader_initable_init;
}

gboolean
bustle_pcap_reader_next (BustlePcapReader  *self,
                         gint64            *timestamp_out,
                         GDBusMessage     **message_out,
                         GError           **error)
{
  g_return_val_if_fail (BUSTLE_IS_PCAP_READER (self), FALSE);
  g_return_val_if_fail (self->savefile != NULL, FALSE);
  g_return_val_if_fail (timestamp_out != NULL, FALSE);
  g_return_val_if_fail (message_out != NULL, FALSE);
  g_return_val_if_fail (*message_out == NULL || G_IS_DBUS_MESSAGE (*message_out), FALSE);
  g_return_val_if_fail (error == NULL || *error == NULL, FALSE);

  /* For ease of looping */
  *timestamp_out = G_MAXINT64;
  g_clear_object (message_out);

  struct pcap_pkthdr *pkt_header;
  const u_char *pkt_data;

  switch (pcap_next_ex (self->savefile, &pkt_header, &pkt_data))
    {
    case 1:
      /* hooray, handled below */
      break;

    case -2:
      /* EOF */
      return TRUE;

    case -1:
      /* error */
      g_set_error (error, G_IO_ERROR, G_IO_ERROR_FAILED,
                   "Reading message from %s failed: %s",
                   self->path,
                   pcap_geterr (self->savefile));
      return FALSE;

    default:
      g_return_val_if_reached (FALSE);
    }

  *message_out = g_dbus_message_new_from_blob ((guchar *) pkt_data,
                                               pkt_header->caplen,
                                               G_DBUS_CAPABILITY_FLAGS_UNIX_FD_PASSING,
                                               error);
  if (*message_out == NULL)
    {
      g_prefix_error (error, "Deserializing message from %s failed: ",
                      self->path);
      return FALSE;
    }

  *timestamp_out = G_USEC_PER_SEC * pkt_header->ts.tv_sec + pkt_header->ts.tv_usec;
  return TRUE;
}
