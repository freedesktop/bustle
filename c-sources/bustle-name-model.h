#pragma once

#include <gio/gio.h>

G_BEGIN_DECLS

#define BUSTLE_TYPE_NAME_MODEL (bustle_name_model_get_type ())

typedef struct _BustleNameModel BustleNameModel;

typedef enum {
  BUSTLE_NAME_MODEL_LANE_STATE_NEW,
  BUSTLE_NAME_MODEL_LANE_STATE_CURRENT,
  BUSTLE_NAME_MODEL_LANE_STATE_CLOSING,

  _BUSTLE_NAME_MODEL_LANE_STATE_LAST = BUSTLE_NAME_MODEL_LANE_STATE_CLOSING
} BustleNameModelLaneState;

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
gboolean bustle_name_model_name_iter_next (BustleNameModelLaneIter   *iter,
                                           const gchar              **unique,
                                           guint                     *lane,
                                           BustleNameModelLaneState  *state);

gboolean bustle_name_model_get_lane (BustleNameModel *self,
                                     const gchar     *name,
                                     guint           *lane);
guint bustle_name_model_get_first_unused_lane (BustleNameModel *self);
guint bustle_name_model_n_unique_names (BustleNameModel *self);


G_DEFINE_AUTOPTR_CLEANUP_FUNC (BustleNameModel, bustle_name_model_unref)

G_END_DECLS
