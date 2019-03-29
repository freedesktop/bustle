#pragma once

#include <gtk/gtk.h>

G_BEGIN_DECLS

#define BUSTLE_TYPE_RECORD_ADDRESS_DIALOG (bustle_record_address_dialog_get_type())

G_DECLARE_FINAL_TYPE (BustleRecordAddressDialog, bustle_record_address_dialog, BUSTLE, RECORD_ADDRESS_DIALOG, GtkDialog)

BustleRecordAddressDialog *bustle_record_address_dialog_new        (GtkWindow                  *parent);
void                       bustle_record_address_dialog_run_async  (BustleRecordAddressDialog  *self,
                                                                    GCancellable               *cancellable,
                                                                    GAsyncReadyCallback         callback,
                                                                    gpointer                    user_data);
gchar                     *bustle_record_address_dialog_run_finish (BustleRecordAddressDialog  *self,
                                                                    GAsyncResult               *result,
                                                                    GError                    **error);



G_END_DECLS
