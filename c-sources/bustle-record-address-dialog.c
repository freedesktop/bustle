#include "bustle-record-address-dialog.h"

struct _BustleRecordAddressDialog
{
  GtkDialog parent_instance;

  GtkEntry *recordAddressEntry;
  GtkButton *recordAddressRecord;
};

G_DEFINE_TYPE (BustleRecordAddressDialog, bustle_record_address_dialog, GTK_TYPE_DIALOG)

BustleRecordAddressDialog *
bustle_record_address_dialog_new (GtkWindow *parent)
{
  return g_object_new (BUSTLE_TYPE_RECORD_ADDRESS_DIALOG,
                       "transient-for", parent,
                       NULL);
}

static void
bustle_record_address_dialog_finalize (GObject *object)
{
  BustleRecordAddressDialog *self = (BustleRecordAddressDialog *)object;

  g_debug ("%s: %p", G_STRFUNC, self);

  G_OBJECT_CLASS (bustle_record_address_dialog_parent_class)->finalize (object);
}

static void
bustle_record_address_dialog_class_init (BustleRecordAddressDialogClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);

  object_class->finalize = bustle_record_address_dialog_finalize;

  gtk_widget_class_set_template_from_resource (widget_class,
                                               "/org/freedesktop/Bustle/bustle-record-address-dialog.ui");
  gtk_widget_class_bind_template_child (widget_class, BustleRecordAddressDialog, recordAddressEntry);
  gtk_widget_class_bind_template_child (widget_class, BustleRecordAddressDialog, recordAddressRecord);
}

static void
entry_changed_cb (GtkEditable *editable,
                  gpointer     user_data)
{
  BustleRecordAddressDialog *self = BUSTLE_RECORD_ADDRESS_DIALOG (user_data);
  GtkEntry *entry = GTK_ENTRY (editable);
  const gchar *text = gtk_entry_get_text (entry);
  g_autoptr(GError) local_error = NULL;
  gboolean ok = text && *text && g_dbus_is_supported_address (text, &local_error);

  gtk_widget_set_sensitive (GTK_WIDGET (self->recordAddressRecord), ok);
  if (ok)
    {
      gtk_entry_set_icon_from_icon_name (entry, GTK_ENTRY_ICON_SECONDARY, NULL);
    }
  else if (text && *text)
    {
      gtk_entry_set_icon_from_icon_name (entry, GTK_ENTRY_ICON_SECONDARY, "dialog-warning");
      gtk_entry_set_icon_tooltip_text (entry, GTK_ENTRY_ICON_SECONDARY, local_error->message);
    }
}

static void
bustle_record_address_dialog_init (BustleRecordAddressDialog *self)
{
  gtk_widget_init_template (GTK_WIDGET (self));

  g_signal_connect_object (self->recordAddressEntry, "changed",
                           G_CALLBACK (entry_changed_cb), self, 0);
  entry_changed_cb (GTK_EDITABLE (self->recordAddressEntry), self);
}

static void
response_cb (GtkDialog *dialog,
             gint       response_id,
             gpointer   user_data)
{
  BustleRecordAddressDialog *self = BUSTLE_RECORD_ADDRESS_DIALOG (dialog);
  g_autoptr(GTask) task = G_TASK (user_data);

  if (response_id == GTK_RESPONSE_ACCEPT)
    g_task_return_pointer (task,
                           g_strdup (gtk_entry_get_text (self->recordAddressEntry)),
                           g_free);
  else
    g_cancellable_cancel (g_task_get_cancellable (task));

  g_signal_handlers_disconnect_by_data (dialog, task);
  gtk_widget_destroy (GTK_WIDGET (dialog));
}


void
bustle_record_address_dialog_run_async  (BustleRecordAddressDialog *self,
                                         GCancellable              *cancellable,
                                         GAsyncReadyCallback        callback,
                                         gpointer                   user_data)
{
  g_autoptr(GTask) task = g_task_new (self, cancellable, callback, user_data);

  g_signal_connect (self, "response",
                    G_CALLBACK (response_cb), g_steal_pointer (&task));
  gtk_widget_show_all (GTK_WIDGET (self));
}

gchar *
bustle_record_address_dialog_run_finish (BustleRecordAddressDialog  *self,
                                         GAsyncResult               *result,
                                         GError                    **error)
{
  g_return_val_if_fail (BUSTLE_IS_RECORD_ADDRESS_DIALOG (self), NULL);
  g_return_val_if_fail (g_task_is_valid (result, self), NULL);
  g_return_val_if_fail (error == NULL || *error == NULL, NULL);

  return g_task_propagate_pointer (G_TASK (result), error);
}
