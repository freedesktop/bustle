#pragma once

#include <glib-object.h>
#include <gio/gio.h>

G_BEGIN_DECLS

#define BUSTLE_TYPE_PCAP_READER (bustle_pcap_reader_get_type())

G_DECLARE_FINAL_TYPE (BustlePcapReader, bustle_pcap_reader, BUSTLE, PCAP_READER, GObject)

BustlePcapReader *bustle_pcap_reader_new  (const gchar       *path,
                                           GError           **error);
gboolean          bustle_pcap_reader_next (BustlePcapReader  *self,
                                           gint64            *timestamp_out,
                                           GDBusMessage     **message_out,
                                           GError           **error);

G_END_DECLS
