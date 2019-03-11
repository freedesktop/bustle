#pragma once

#include <gtk/gtk.h>

G_BEGIN_DECLS

#define BUSTLE_TYPE_MODEL (bustle_model_get_type())

G_DECLARE_FINAL_TYPE (BustleModel, bustle_model, BUSTLE, MODEL, GObject)

BustleModel  *bustle_model_new            (void);
GtkTreeModel *bustle_model_get_tree_model (BustleModel  *self);
void          bustle_model_add_message    (BustleModel  *self,
                                           gint64        timestamp,
                                           GDBusMessage *message);

typedef enum {
  /* gint64 */
  BUSTLE_MODEL_COLUMN_TIMESTAMP_USEC,
  /* GDBusMessage */
  BUSTLE_MODEL_COLUMN_DBUS_MESSAGE,
  /* nullable GtkTreeRowReference */
  BUSTLE_MODEL_COLUMN_COUNTERPART,
  /* gboolean */
  BUSTLE_MODEL_COLUMN_DUPLICATE_REPLY,
  /* BustleNameModel */
  BUSTLE_MODEL_COLUMN_NAME_MODEL,
  /* gchar ** */
  BUSTLE_MODEL_COLUMN_SEARCH_TOKENS,
} BustleModelColumn;


typedef struct _BustleModelSearchData BustleModelSearchData;

BustleModelSearchData *bustle_model_search_data_new   (void);
void                   bustle_model_search_data_free  (BustleModelSearchData *data);
gboolean               bustle_model_search_equal_func (GtkTreeModel          *model,
                                                       gint                   column,
                                                       const gchar           *key,
                                                       GtkTreeIter           *iter,
                                                       gpointer               search_data);

G_END_DECLS
