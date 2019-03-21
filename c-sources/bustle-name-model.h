#pragma once

#include <gio/gio.h>

G_BEGIN_DECLS

#define BUSTLE_TYPE_NAME_MODEL (bustle_name_model_get_type ())

typedef struct _BustleNameModel BustleNameModel;

typedef GHashTableIter BustleNameModelLaneIter;

GType            bustle_name_model_get_type (void) G_GNUC_CONST;
BustleNameModel *bustle_name_model_new      (void);
BustleNameModel *bustle_name_model_ref      (BustleNameModel *self);
void             bustle_name_model_unref    (BustleNameModel *self);
void             bustle_name_model_update   (BustleNameModel **self,
                                             GDBusMessage     *message,
                                             GDBusMessage     *counterpart);

void bustle_name_model_name_iter_init (BustleNameModel         *self,
                                       BustleNameModelLaneIter *iter);
gboolean bustle_name_model_name_iter_next (BustleNameModelLaneIter  *iter,
                                           const gchar             **unique,
                                           guint                    *lane);

gboolean bustle_name_model_has_unique_name (BustleNameModel *self,
                                            const gchar     *unique);
gboolean bustle_name_model_get_lane (BustleNameModel *self,
                                     const gchar     *name,
                                     guint32         *lane);
guint32 bustle_name_model_get_first_unused_lane (BustleNameModel *self);
guint bustle_name_model_n_unique_names (BustleNameModel *self);


G_DEFINE_AUTOPTR_CLEANUP_FUNC (BustleNameModel, bustle_name_model_unref)

G_END_DECLS
