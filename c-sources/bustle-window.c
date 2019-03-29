#include "bustle-window.h"

#include <glib/gi18n.h>

#include "bustle-cell-renderer-chart.h"
#include "bustle-model.h"
#include "bustle-name-model.h"
#include "bustle-pcap-reader.h"
#include "bustle-record-address-dialog.h"
#include "pcap-monitor.h"

struct _BustleWindow
{
  GtkApplicationWindow parent_instance;

  GFile *file;
  BustlePcapMonitor *monitor;

  BustleModel *model;
  guint messages_logged;
  /* Timestamp of the first message in @model, or 0 if it is empty. */
  gint64 first_ts;

  GtkStack *diagramOrNot;
  GtkScrolledWindow *diagramScrolledWindow;

  /* Error stuff */
  GtkInfoBar *errorBar;
  GtkLabel *errorBarTitle;
  GtkLabel *errorBarDetails;

  /* Menu stuff */
  GtkMenuButton *headerRecord;
  GtkMenuButton *headerStop;
  /* TODO: move to actions */
  GtkMenuItem *recordSession;
  GtkMenuItem *recordSystem;
  GtkMenuItem *recordAddress;

  GtkSpinner *headerSpinner;
  GtkLabel *headerTitle;
  GtkLabel *headerSubtitle;

  /* Details stuff */
  /* TODO: move to separate widget */
  GtkGrid *detailsGrid;
  GtkStack *detailsType;
  GtkLabel *detailsSender;
  GtkLabel *detailsDestinationCaption;
  GtkLabel *detailsDestination;
  GtkLabel *detailsPathCaption;
  GtkLabel *detailsPath;
  GtkLabel *detailsMemberCaption;
  GtkLabel *detailsMember;
  GtkLabel *detailsErrorNameCaption;
  GtkLabel *detailsErrorName;
  GtkTextView *detailsArguments;
};

G_DEFINE_TYPE (BustleWindow, bustle_window, GTK_TYPE_APPLICATION_WINDOW)

static void bustle_window_show_model (BustleWindow *self);
static void bustle_window_show_error (BustleWindow *self,
                                      const gchar  *title,
                                      const GError *error);

static void record_cb (GtkMenuItem *item,
                       gpointer     user_data);
static void stop_cb   (GtkButton   *item,
                       gpointer     user_data);

typedef enum {
  PROP_FILE = 1,
  N_PROPS
} BustleWindowProperty;

static GParamSpec *properties [N_PROPS];

BustleWindow *
bustle_window_new (GtkApplication *application)
{
  g_return_val_if_fail (GTK_IS_APPLICATION (application), NULL);

  return g_object_new (BUSTLE_TYPE_WINDOW,
                       "application", application,
                       NULL);
}

static void
error_bar_response_cb (GtkInfoBar *info_bar,
                       gint        response_id,
                       gpointer    user_data)
{
  gtk_widget_hide (GTK_WIDGET (info_bar));
}

static void
bustle_window_constructed (GObject *object)
{
  BustleWindow *self = (BustleWindow *)object;

  G_OBJECT_CLASS (bustle_window_parent_class)->constructed (object);

  self->model = bustle_model_new ();
  bustle_window_show_model (self);

  g_signal_connect_object (self->errorBar, "response",
                           G_CALLBACK (error_bar_response_cb), self,
                           0);

  g_signal_connect (self->recordSession, "activate", G_CALLBACK (record_cb), self);
  g_signal_connect (self->recordSystem, "activate", G_CALLBACK (record_cb), self);
  g_signal_connect (self->recordAddress, "activate", G_CALLBACK (record_cb), self);
  g_signal_connect (self->headerStop, "clicked", G_CALLBACK (stop_cb), self);

  gtk_widget_show (GTK_WIDGET (self));
}

static void
bustle_window_finalize (GObject *object)
{
  BustleWindow *self = (BustleWindow *)object;

  g_clear_object (&self->file);
  g_clear_object (&self->monitor);
  g_clear_object (&self->model);

  G_OBJECT_CLASS (bustle_window_parent_class)->finalize (object);
}

static void
bustle_window_get_property (GObject    *object,
                            guint       prop_id,
                            GValue     *value,
                            GParamSpec *pspec)
{
  BustleWindow *self = BUSTLE_WINDOW (object);

  switch ((BustleWindowProperty) prop_id)
    {
    case PROP_FILE:
      g_value_set_object (value, self->file);
      break;

    case N_PROPS:
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
    }
}

static void
bustle_window_set_property (GObject      *object,
                            guint         prop_id,
                            const GValue *value,
                            GParamSpec   *pspec)
{
  BustleWindow *self = BUSTLE_WINDOW (object);

  switch ((BustleWindowProperty) prop_id)
    {
    case PROP_FILE:
      g_set_object (&self->file, G_FILE (g_value_dup_object (value)));
      break;

    case N_PROPS:
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
    }
}

static void
bustle_window_class_init (BustleWindowClass *klass)
{
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->constructed = bustle_window_constructed;
  object_class->finalize = bustle_window_finalize;
  object_class->get_property = bustle_window_get_property;
  object_class->set_property = bustle_window_set_property;

  properties [PROP_FILE] =
    g_param_spec_object ("file",
                         "File",
                         "File",
                         G_TYPE_FILE,
                         (G_PARAM_READABLE |
                          G_PARAM_STATIC_STRINGS));
  g_object_class_install_property (object_class, PROP_FILE,
                                   properties [PROP_FILE]);

  gtk_widget_class_set_template_from_resource (widget_class,
                                               "/org/freedesktop/Bustle/bustle-window.ui");
  gtk_widget_class_bind_template_child (widget_class, BustleWindow, diagramOrNot);
  gtk_widget_class_bind_template_child (widget_class, BustleWindow, diagramScrolledWindow);

  gtk_widget_class_bind_template_child (widget_class, BustleWindow, errorBar);
  gtk_widget_class_bind_template_child (widget_class, BustleWindow, errorBarTitle);
  gtk_widget_class_bind_template_child (widget_class, BustleWindow, errorBarDetails);

  gtk_widget_class_bind_template_child (widget_class, BustleWindow, headerRecord);
  gtk_widget_class_bind_template_child (widget_class, BustleWindow, headerStop);
  gtk_widget_class_bind_template_child (widget_class, BustleWindow, recordSession);
  gtk_widget_class_bind_template_child (widget_class, BustleWindow, recordSystem);
  gtk_widget_class_bind_template_child (widget_class, BustleWindow, recordAddress);

  gtk_widget_class_bind_template_child (widget_class, BustleWindow, headerSpinner);
  gtk_widget_class_bind_template_child (widget_class, BustleWindow, headerTitle);
  gtk_widget_class_bind_template_child (widget_class, BustleWindow, headerSubtitle);

  gtk_widget_class_bind_template_child (widget_class, BustleWindow, detailsGrid);
  gtk_widget_class_bind_template_child (widget_class, BustleWindow, detailsType);
  gtk_widget_class_bind_template_child (widget_class, BustleWindow, detailsSender);
  gtk_widget_class_bind_template_child (widget_class, BustleWindow, detailsDestinationCaption);
  gtk_widget_class_bind_template_child (widget_class, BustleWindow, detailsDestination);
  gtk_widget_class_bind_template_child (widget_class, BustleWindow, detailsPathCaption);
  gtk_widget_class_bind_template_child (widget_class, BustleWindow, detailsPath);
  gtk_widget_class_bind_template_child (widget_class, BustleWindow, detailsMemberCaption);
  gtk_widget_class_bind_template_child (widget_class, BustleWindow, detailsMember);
  gtk_widget_class_bind_template_child (widget_class, BustleWindow, detailsErrorNameCaption);
  gtk_widget_class_bind_template_child (widget_class, BustleWindow, detailsErrorName);
  gtk_widget_class_bind_template_child (widget_class, BustleWindow, detailsArguments);
}

static void
bustle_window_init (BustleWindow *self)
{
  gtk_widget_init_template (GTK_WIDGET (self));
}

static void
timestamp_data_func (GtkTreeViewColumn *tree_column,
                     GtkCellRenderer   *cell,
                     GtkTreeModel      *tree_model,
                     GtkTreeIter       *iter,
                     gpointer           data)
{
  BustleWindow *self = BUSTLE_WINDOW (data);
  gint64 timestamp_usec;
  g_autofree gchar *text = NULL;

  gtk_tree_model_get (tree_model, iter,
                      BUSTLE_MODEL_COLUMN_TIMESTAMP_USEC, &timestamp_usec,
                      -1);
  text = g_strdup_printf ("+%.3fs", (double) (timestamp_usec - self->first_ts) / G_USEC_PER_SEC);
  g_object_set (cell, "text", text, NULL);
}

static void
services_data_func (GtkTreeViewColumn *tree_column,
                    GtkCellRenderer   *cell,
                    GtkTreeModel      *tree_model,
                    GtkTreeIter       *iter,
                    gpointer           data)
{
  g_autoptr(GDBusMessage) message = NULL;
  g_autofree gchar *text = NULL;

  gtk_tree_model_get (tree_model, iter,
                      BUSTLE_MODEL_COLUMN_DBUS_MESSAGE, &message,
                      -1);
  const gchar *sender = g_dbus_message_get_sender (message);
  const gchar *destination = g_dbus_message_get_destination (message);
  /* TODO: find "nice" names? Maybe it's better to tell the truth. */

  if (sender == NULL && destination == NULL)
    text = g_strdup_printf ("<i>Unknown</i>");
  else if (destination == NULL)
    text = g_strdup_printf ("⇐ %s", sender);
  else if (sender == NULL)
    /* weird */
    text = g_strdup_printf ("⇐ <i>Unknown</i>\n⇒ %s", destination);
  else
    text = g_strdup_printf ("⇐ %s\n⇒ %s", sender, destination);

  g_object_set (cell, "markup", text, NULL);
}

static void
get_fields (GDBusMessage         *message,
            GtkTreeRowReference  *counterpart_ref,
            gboolean             *is_reply,
            const gchar         **path_out,
            const gchar         **interface_out,
            const gchar         **member_out)
{
  switch (g_dbus_message_get_message_type (message))
    {
    case G_DBUS_MESSAGE_TYPE_SIGNAL:
    case G_DBUS_MESSAGE_TYPE_METHOD_CALL:
      *is_reply = FALSE;
      *path_out = g_dbus_message_get_path (message);
      *interface_out = g_dbus_message_get_interface (message);
      *member_out = g_dbus_message_get_member (message);
      break;

    case G_DBUS_MESSAGE_TYPE_METHOD_RETURN:
    case G_DBUS_MESSAGE_TYPE_ERROR:
      *is_reply = TRUE;
      if (counterpart_ref != NULL)
        {
          GtkTreeModel *model = gtk_tree_row_reference_get_model (counterpart_ref);
          g_autoptr(GtkTreePath) path = gtk_tree_row_reference_get_path (counterpart_ref);
          GtkTreeIter iter;
          g_autoptr(GDBusMessage) counterpart = NULL;

          gtk_tree_model_get_iter (model, &iter, path);
          gtk_tree_model_get (model, &iter,
                              BUSTLE_MODEL_COLUMN_DBUS_MESSAGE, &counterpart,
                              -1);
          *path_out = g_dbus_message_get_path (counterpart);
          *interface_out = g_dbus_message_get_interface (counterpart);
          *member_out = g_dbus_message_get_member (counterpart);
        }
      else
        {
          *path_out = NULL;
          *interface_out = NULL;
          *member_out = NULL;
        }
      break;

    default:
      g_return_if_reached ();
    }
}

static gchar *
format_member (GDBusMessage        *message,
               GtkTreeRowReference *counterpart_ref)
{
  gboolean is_reply;
  const gchar *path, *interface, *member;

  get_fields (message, counterpart_ref, &is_reply, &path, &interface, &member);

  if (!is_reply)
    {
      return g_markup_printf_escaped ("<b>%s</b>\n%s.<b>%s</b>",
                                      path, interface, member);
    }
  else if (path != NULL)
    {
      return g_markup_printf_escaped ("%s\n<i>%s.<b>%s</b></i>",
                                      path, interface, member);

    }
  else
    {
      return g_strdup_printf ("<i>%s</i>",
                              _("Unknown"));
    }
}

static void
member_data_func (GtkTreeViewColumn *tree_column,
                  GtkCellRenderer   *cell,
                  GtkTreeModel      *tree_model,
                  GtkTreeIter       *iter,
                  gpointer           data)
{
  g_autoptr(GDBusMessage) message = NULL;
  g_autoptr(GtkTreeRowReference) ref = NULL;
  g_autofree gchar *text = NULL;

  gtk_tree_model_get (tree_model, iter,
                      BUSTLE_MODEL_COLUMN_DBUS_MESSAGE, &message,
                      BUSTLE_MODEL_COLUMN_COUNTERPART, &ref,
                      -1);

  text = format_member (message, ref);
  g_object_set (cell, "markup", text, NULL);
}

static void
chart_data_func (GtkTreeViewColumn *tree_column,
                 GtkCellRenderer   *cell,
                 GtkTreeModel      *tree_model,
                 GtkTreeIter       *iter,
                 gpointer           data)
{
  g_autoptr(GDBusMessage) message = NULL;
  g_autoptr(BustleNameModel) name_model = NULL;
  g_autoptr(BustleNameModel) name_model_prev = NULL;
  g_autoptr(BustleNameModel) name_model_next = NULL;
  GtkTreeIter sibling_iter;

  gtk_tree_model_get (tree_model, iter,
                      BUSTLE_MODEL_COLUMN_DBUS_MESSAGE, &message,
                      BUSTLE_MODEL_COLUMN_NAME_MODEL, &name_model,
                      -1);

  sibling_iter = *iter;
  if (gtk_tree_model_iter_previous (tree_model, &sibling_iter))
    gtk_tree_model_get (tree_model, &sibling_iter,
                        BUSTLE_MODEL_COLUMN_NAME_MODEL, &name_model_prev,
                        -1);
  sibling_iter = *iter;
  if (gtk_tree_model_iter_next (tree_model, &sibling_iter))
    gtk_tree_model_get (tree_model, &sibling_iter,
                        BUSTLE_MODEL_COLUMN_NAME_MODEL, &name_model_next,
                        -1);

  g_object_set (cell,
                "dbus-message", message,
                "name-model-prev", name_model_prev,
                "name-model", name_model,
                "name-model-next", name_model_next,
                NULL);
}

static void
debug_data_func (GtkTreeViewColumn *tree_column,
                 GtkCellRenderer   *cell,
                 GtkTreeModel      *tree_model,
                 GtkTreeIter       *iter,
                 gpointer           data)
{
  g_autoptr(GDBusMessage) message = NULL;
  g_autoptr(BustleNameModel) name_model = NULL;
  g_autofree gchar *text = NULL;

  gtk_tree_model_get (tree_model, iter,
                      BUSTLE_MODEL_COLUMN_DBUS_MESSAGE, &message,
                      BUSTLE_MODEL_COLUMN_NAME_MODEL, &name_model,
                      -1);
  if (g_strcmp0 (g_dbus_message_get_member (message), "NameOwnerChanged"))
    text = g_dbus_message_print (message, 2);

  g_object_set (cell, "text", text, NULL);
}

static const gchar *
details_type_name (GDBusMessage *message)
{
  switch (g_dbus_message_get_message_type (message))
    {
    case G_DBUS_MESSAGE_TYPE_METHOD_CALL:
      return "methodCall";
    case G_DBUS_MESSAGE_TYPE_SIGNAL:
      if (g_dbus_message_get_destination (message) != NULL)
        return "directedSignal";
      else
        return "signal";
    case G_DBUS_MESSAGE_TYPE_METHOD_RETURN:
      return "methodReturn";
    case G_DBUS_MESSAGE_TYPE_ERROR:
      return "error";
    default:
      g_return_val_if_reached (NULL);
    }
}

static void
details_set_optional_row (GtkLabel *caption,
                          GtkLabel *label,
                          const gchar *text)
{
  gtk_widget_set_visible (GTK_WIDGET (caption), text != NULL);
  gtk_widget_set_visible (GTK_WIDGET (label), text != NULL);
  gtk_label_set_text (label, text);
}

static void
selection_changed_cb (GtkTreeSelection *selection,
                      gpointer           user_data)
{
  BustleWindow *self = BUSTLE_WINDOW (user_data);
  GtkTreeModel *model;
  GtkTreeIter iter;
  g_autoptr(GDBusMessage) message = NULL;
  g_autoptr(GtkTreeRowReference) counterpart_ref = NULL;
  gboolean is_reply;
  const gchar *path, *interface, *member;
  g_autofree gchar *full_member = NULL;

  if (!gtk_tree_selection_get_selected (selection, &model, &iter))
    {
      gtk_widget_hide (GTK_WIDGET (self->detailsGrid));
      return;
    }

  gtk_tree_model_get (model, &iter,
                      BUSTLE_MODEL_COLUMN_DBUS_MESSAGE, &message,
                      BUSTLE_MODEL_COLUMN_COUNTERPART, &counterpart_ref,
                      -1);
  get_fields (message, counterpart_ref, &is_reply, &path, &interface, &member);
  if (interface != NULL || member != NULL)
    {
      if (interface == NULL)
        full_member = g_strdup (member);
      else
        full_member = g_strdup_printf ("%s.%s",
                                       interface,
                                       member ?: _("(unknown)"));
    }

  gtk_stack_set_visible_child_name (self->detailsType,
                                    details_type_name (message));
  /* TODO: these would be a lot more useful if we could resolve unique names
   * to/from well-known names and show both
   */
  gtk_label_set_text (self->detailsSender,
                      g_dbus_message_get_sender (message));
  details_set_optional_row (self->detailsDestinationCaption,
                            self->detailsDestination,
                            g_dbus_message_get_destination (message));
  details_set_optional_row (self->detailsPathCaption,
                            self->detailsPath,
                            path);
  details_set_optional_row (self->detailsMemberCaption,
                            self->detailsMember,
                            full_member);
  details_set_optional_row (self->detailsErrorNameCaption,
                            self->detailsErrorName,
                            g_dbus_message_get_error_name (message));
  GVariant *body = g_dbus_message_get_body (message);
  const gchar *body_str = "";
  g_autofree gchar *body_str_to_free = NULL;
  if (body != NULL)
    body_str = body_str_to_free = g_variant_print (body, TRUE);
  gtk_text_buffer_set_text (gtk_text_view_get_buffer (self->detailsArguments),
                            body_str, -1);
  gtk_widget_show (GTK_WIDGET (self->detailsGrid));
}

static void
bustle_window_show_model (BustleWindow *self)
{
  /*
   */

  GtkTreeModel *model = bustle_model_get_tree_model (self->model);
  GtkWidget *tree_view = gtk_tree_view_new_with_model (model);
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;

  gtk_tree_view_set_search_column (GTK_TREE_VIEW (tree_view), BUSTLE_MODEL_COLUMN_SEARCH_TOKENS);
  gtk_tree_view_set_search_equal_func (GTK_TREE_VIEW (tree_view),
                                       bustle_model_search_equal_func,
                                       bustle_model_search_data_new (),
                                       (GDestroyNotify) bustle_model_search_data_free);

  renderer = gtk_cell_renderer_text_new ();
  g_object_set (renderer, "xalign", (gdouble) 1.0, NULL);
  column = gtk_tree_view_column_new_with_attributes ("Timestamp", renderer, NULL);
  gtk_tree_view_column_set_cell_data_func (column, renderer,
                                           timestamp_data_func,
                                           self, NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (tree_view), column);

  renderer = gtk_cell_renderer_text_new ();
  column = gtk_tree_view_column_new_with_attributes ("Services", renderer, NULL);
  gtk_tree_view_column_set_cell_data_func (column, renderer, services_data_func, NULL, NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (tree_view), column);

  renderer = gtk_cell_renderer_text_new ();
  column = gtk_tree_view_column_new_with_attributes ("Member", renderer, NULL);
  gtk_tree_view_column_set_spacing (column, 12);
  gtk_tree_view_column_set_cell_data_func (column, renderer, member_data_func, NULL, NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (tree_view), column);
  renderer = gtk_cell_renderer_pixbuf_new ();
  g_object_set (renderer, "icon-name", "dialog-warning-symbolic", NULL);
  gtk_tree_view_column_pack_end (column, renderer, FALSE);
  gtk_tree_view_column_set_attributes (column, renderer,
                                       "visible", BUSTLE_MODEL_COLUMN_DUPLICATE_REPLY,
                                       NULL);

  renderer = bustle_cell_renderer_chart_new ();
  column = gtk_tree_view_column_new_with_attributes ("Chart", renderer, NULL);
  gtk_tree_view_column_set_cell_data_func (column, renderer, chart_data_func, NULL, NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (tree_view), column);

  if (0)
    {
      renderer = gtk_cell_renderer_text_new ();
      column = gtk_tree_view_column_new_with_attributes ("Debug", renderer, NULL);
      gtk_tree_view_column_set_cell_data_func (column, renderer, debug_data_func, NULL, NULL);
      gtk_tree_view_append_column (GTK_TREE_VIEW (tree_view), column);
    }

  GtkTreeSelection *selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (tree_view));
  g_signal_connect (selection, "changed",
                    G_CALLBACK (selection_changed_cb), self);
  gtk_tree_selection_unselect_all (selection);
  selection_changed_cb (selection, self);

  gtk_container_add (GTK_CONTAINER (self->diagramScrolledWindow), tree_view);
  gtk_widget_show (tree_view);
}

static void
bustle_window_add_message (BustleWindow *self,
                           gint64        ts,
                           GDBusMessage *message)
{
  if (self->first_ts == 0)
    {
      self->first_ts = ts;
      gtk_stack_set_visible_child_name (self->diagramOrNot, "CanvasPage");
    }

  bustle_model_add_message (self->model, ts, message);
}

static gboolean
bustle_window_do_load_file (BustleWindow  *self,
                            GError       **error)
{
  g_assert (self->file != NULL);

  const gchar *path = g_file_peek_path (self->file);
  g_autoptr(BustlePcapReader) reader = bustle_pcap_reader_new (path, error);
  if (reader == NULL)
    return FALSE;

  gint64 ts;
  g_autoptr(GDBusMessage) message = NULL;

  while (bustle_pcap_reader_next (reader, &ts, &message, error))
    {
      if (message == NULL)
        {
          /* EOF */
          return TRUE;
        }

      bustle_window_add_message (self, ts, message);
    }

  return FALSE;
}

static void
bustle_window_show_error (BustleWindow *self,
                          const gchar  *title,
                          const GError *error)
{
  g_assert (BUSTLE_IS_WINDOW (self));
  g_assert (title != NULL);
  g_assert (error == NULL || error->message != NULL);

  if (g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
    return;

  g_autofree gchar *title_markup = g_markup_printf_escaped ("<b>%s</b>", title);
  gtk_label_set_markup (self->errorBarTitle, title_markup);

  gtk_widget_set_visible (GTK_WIDGET (self->errorBarDetails), error != NULL);
  gtk_label_set_text (self->errorBarDetails, error != NULL ? error->message : "");

  /* TODO: use :revealed, modulo https://gitlab.gnome.org/GNOME/gtk/issues/1165 */
  gtk_widget_show (GTK_WIDGET (self->errorBar));
}

void
bustle_window_load_file (BustleWindow *self,
                         GFile        *file)
{
  g_return_if_fail (BUSTLE_IS_WINDOW (self));
  g_return_if_fail (G_IS_FILE (file));

  g_return_if_fail (self->file == NULL);
  self->file = g_object_ref (file);
  g_object_notify_by_pspec (G_OBJECT (self), properties[PROP_FILE]);

  g_autoptr(GError) local_error = NULL;

  if (!bustle_window_do_load_file (self, &local_error))
    {
      g_autofree gchar *title = g_strdup_printf (_("Could not read ‘%s’."),
                                                 g_file_peek_path (self->file));
      bustle_window_show_error (self, title, local_error);
    }
}

static void
message_logged_cb (BustlePcapMonitor *monitor,
                   long               timestamp_sec,
                   long               timestamp_usec,
                   const guint8      *blob,
                   gsize              len,
                   gpointer           user_data)
{
  BustleWindow *self = BUSTLE_WINDOW (user_data);
  gint64 timestamp = G_USEC_PER_SEC * timestamp_sec + timestamp_usec;
  g_autoptr(GError) local_error = NULL;
  g_autoptr(GDBusMessage) message = NULL;

  message = g_dbus_message_new_from_blob ((guchar *) blob, len,
                                          G_DBUS_CAPABILITY_FLAGS_UNIX_FD_PASSING,
                                          &local_error);
  if (message == NULL)
    {
      bustle_window_show_error (self,
                                _("Could not decode recorded message."),
                                local_error);
      bustle_pcap_monitor_stop (self->monitor);
    }
  else
    {
      bustle_window_add_message (self, timestamp, message);
      self->messages_logged++;

      g_autofree gchar *subtitle = g_strdup_printf (_("%u messages"),
                                                    self->messages_logged);
      gtk_label_set_text (self->headerSubtitle, subtitle);
    }
}

static void
stopped_cb (BustlePcapMonitor *monitor,
            guint              domain,
            gint               code,
            const gchar       *message,
            gpointer           user_data)
{
  BustleWindow *self = BUSTLE_WINDOW (user_data);
  g_assert (domain != 0);
  g_assert (domain <= G_MAXUINT32);
  g_autoptr(GError) local_error = g_error_new_literal ((GQuark) domain, code, message);

  gtk_spinner_stop (self->headerSpinner);
  gtk_widget_show (GTK_WIDGET (self->headerRecord));
  gtk_widget_hide (GTK_WIDGET (self->headerStop));
  if (self->first_ts == 0)
    {
      gtk_stack_set_visible_child_name (self->diagramOrNot, "InstructionsPage");
      g_clear_object (&self->file);
    }

  if (!g_error_matches (local_error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
    bustle_window_show_error (self,
                              _("Recording failed."),
                              local_error);

  g_signal_handlers_disconnect_by_data (self->monitor, self);
  g_clear_object (&self->monitor);
}

static void
bustle_window_start_recording (BustleWindow *self,
                               GBusType      bus_type,
                               const gchar  *address)
{
  g_assert (BUSTLE_IS_WINDOW (self));
  g_assert ((bus_type == G_BUS_TYPE_NONE) == (address != NULL));
  g_assert (self->file == NULL);
  g_assert (self->monitor == NULL);

  g_autoptr(GDateTime) now = g_date_time_new_now_local ();
  g_autofree gchar *basename = g_date_time_format (now,
                                                   "%F %H-%M-%S.pcap");
  g_autofree gchar *target_path = g_build_filename (g_get_user_cache_dir (),
                                                    "bustle",
                                                    basename,
                                                    NULL);
  g_autoptr(GFile) target_file = g_file_new_for_path (target_path);
  g_autoptr(GFile) target_dir = g_file_get_parent (target_file);
  g_autoptr(GError) local_error = NULL;

  if (!g_file_make_directory_with_parents (target_dir, NULL, &local_error) &&
      !g_error_matches (local_error, G_IO_ERROR, G_IO_ERROR_EXISTS))
    {
      bustle_window_show_error (self,
                                _("Could not start recording."),
                                local_error);
      return;
    }

  g_clear_error (&local_error);
  self->monitor = bustle_pcap_monitor_new (bus_type, address, target_path,
                                           &local_error);
  if (self->monitor == NULL)
    {
      bustle_window_show_error (self,
                                _("Could not start recording."),
                                local_error);
    }
  else
    {
      g_autofree gchar *title_to_free = NULL;
      const gchar *title = NULL;
      switch (bus_type)
        {
        case G_BUS_TYPE_SESSION:
          title = _("Recording session bus…");
          break;
        case G_BUS_TYPE_SYSTEM:
          title = _("Recording system bus…");
          break;
        case G_BUS_TYPE_NONE:
          title_to_free = g_strdup_printf (_("Recording %s…"), address);
          title = title_to_free;
          break;
        default:
          g_assert_not_reached ();
        }

      g_set_object (&self->file, target_file);

      gtk_stack_set_visible_child_name (self->diagramOrNot, "PleaseHoldPage");

      gtk_spinner_start (self->headerSpinner);
      gtk_label_set_text (self->headerTitle, title);

      gtk_widget_hide (GTK_WIDGET (self->headerRecord));
      gtk_widget_show (GTK_WIDGET (self->headerStop));

      g_signal_connect_object (self->monitor, "message-logged",
                               G_CALLBACK (message_logged_cb), self, 0);
      g_signal_connect_object (self->monitor, "stopped",
                               G_CALLBACK (stopped_cb), self, 0);
    }
}

static void
bustle_window_start_recording_somewhere (BustleWindow *self,
                                         GBusType      bus_type,
                                         const gchar  *address)
{
  BustleWindow *target;

  if (self->first_ts != 0)
    target = bustle_window_new (gtk_window_get_application (GTK_WINDOW (self)));
  else
    target = self;

  bustle_window_start_recording (target, bus_type, address);
}

static void
record_address_cb (GObject      *source,
                   GAsyncResult *result,
                   gpointer      user_data)
{
  g_autoptr(BustleWindow) self = BUSTLE_WINDOW (user_data);
  g_autoptr(GError) local_error = NULL;
  g_autofree gchar *address = NULL;

  address = bustle_record_address_dialog_run_finish (BUSTLE_RECORD_ADDRESS_DIALOG (source),
                                                     result,
                                                     &local_error);
  if (address != NULL)
    bustle_window_start_recording_somewhere (self, G_BUS_TYPE_NONE, address);
  else if (!g_error_matches (local_error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
    g_warning ("%s", local_error->message);
}

static void
record_cb (GtkMenuItem *item,
           gpointer     user_data)
{
  BustleWindow *self = BUSTLE_WINDOW (user_data);

  if (self->recordSession == item)
    bustle_window_start_recording_somewhere (self, G_BUS_TYPE_SESSION, NULL);
  else if (self->recordSystem == item)
    bustle_window_start_recording_somewhere (self, G_BUS_TYPE_SYSTEM, NULL);
  else if (self->recordAddress == item)
    {
      BustleRecordAddressDialog *d = bustle_record_address_dialog_new (GTK_WINDOW (self));
      bustle_record_address_dialog_run_async (d, NULL, record_address_cb, g_object_ref (self));
    }
  else
    g_assert_not_reached ();
}

static void
stop_cb (GtkButton *item,
         gpointer   user_data)
{
  BustleWindow *self = BUSTLE_WINDOW (user_data);

  g_assert (self->monitor != NULL);

  bustle_pcap_monitor_stop (self->monitor);
}
