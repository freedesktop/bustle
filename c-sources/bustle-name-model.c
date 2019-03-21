#include "bustle-name-model.h"

G_DEFINE_BOXED_TYPE (BustleNameModel, bustle_name_model, bustle_name_model_ref, bustle_name_model_unref)

#define DAEMON_NAME "org.freedesktop.DBus"
#define DAEMON_INTERFACE "org.freedesktop.DBus"

#define MAX_LANE (((guint32) 2 << 30) - 1)

typedef enum {
  BUSTLE_NAME_MODEL_LANE_STATE_CURRENT = 0,
  BUSTLE_NAME_MODEL_LANE_STATE_CLOSING = 1,
} BustleNameModelLaneState;

static gboolean
name_is_unique (const gchar *name)
{
  g_return_val_if_fail (name != NULL, FALSE);

  return name[0] == ':' || 0 == g_strcmp0 (DAEMON_NAME, name);
}

static inline gpointer
bustle_name_model_lane_pack (guint32                  lane,
                             BustleNameModelLaneState state)
{
  g_assert (lane <= MAX_LANE);
  g_assert (0 <= state);
  g_assert (state < 2);

  return GUINT_TO_POINTER ((lane << 1) | (1 & (guint) state));
}

static inline void
bustle_name_model_lane_unpack (gpointer                  packed,
                               guint                    *lane,
                               BustleNameModelLaneState *state)
{
  guint packed_ = GPOINTER_TO_UINT (packed);

  if (lane != NULL)
    *lane = packed_ >> 1;
  if (state != NULL)
    *state = packed_ & 1;
}
struct _BustleNameModel
{
  /*< private >*/
  guint ref_count;

  /* unique -> GHashTable-as-set<well_known> */
  GHashTable *owned_names;

  /* well-known -> unique */
  GHashTable *name_owners;

  /* unique -> bustle_name_model_lane_pack (lane, state) */
  GHashTable *lanes;

  /* Next free lane */
  guint32 next_lane;
};


/**
 * bustle_name_model_new:
 *
 * Creates a new #BustleNameModel.
 *
 * Returns: (transfer full): A newly created #BustleNameModel
 */
BustleNameModel *
bustle_name_model_new (void)
{
  BustleNameModel *self;

  self = g_slice_new0 (BustleNameModel);
  self->ref_count = 1;
  /* TODO: either make the names GRefStrings, or intern them ourselves */
  self->owned_names = g_hash_table_new_full (g_str_hash, g_str_equal,
                                             NULL, (GDestroyNotify) g_hash_table_unref);
  self->name_owners = g_hash_table_new (g_str_hash, g_str_equal);
  self->lanes = g_hash_table_new (g_str_hash, g_str_equal);

  return self;
}

/**
 * bustle_name_model_copy:
 * @self: (inout) (transfer full): a #BustleNameModel
 *
 * Makes a deep copy of @self, consuming that reference. (If that was the last
 * reference, no copy is made.)
 *
 * TODO: fix docs
 */
static void
bustle_name_model_copy (BustleNameModel **self)
{
  BustleNameModel *copy;

  g_return_if_fail (self != NULL);
  g_return_if_fail (*self != NULL);
  g_return_if_fail ((*self)->ref_count);

  if (g_atomic_int_get (&(*self)->ref_count) == 1)
    {
      /* No other refs, nothing to do */
      return;
    }

  copy = bustle_name_model_new ();

  GHashTableIter iter;
  gpointer key, value;

  /* TODO: do this more lazily? */
  g_hash_table_iter_init (&iter, (*self)->owned_names);
  while (g_hash_table_iter_next (&iter, &key, &value))
    g_hash_table_insert (copy->owned_names, key, g_hash_table_ref (value));

  g_hash_table_iter_init (&iter, (*self)->name_owners);
  while (g_hash_table_iter_next (&iter, &key, &value))
    g_hash_table_insert (copy->name_owners, key, value);

  g_hash_table_iter_init (&iter, (*self)->lanes);
  while (g_hash_table_iter_next (&iter, &key, &value))
    g_hash_table_insert (copy->lanes, key, value);

  copy->next_lane = (*self)->next_lane;

  bustle_name_model_unref (*self);
  *self = copy;
}

static void
bustle_name_model_free (BustleNameModel *self)
{
  g_assert (self);
  g_assert_cmpint (self->ref_count, ==, 0);

  g_hash_table_unref (self->owned_names);
  g_hash_table_unref (self->name_owners);
  g_slice_free (BustleNameModel, self);
}

/**
 * bustle_name_model_ref:
 * @self: A #BustleNameModel
 *
 * Increments the reference count of @self by one.
 *
 * Returns: (transfer none): @self
 */
BustleNameModel *
bustle_name_model_ref (BustleNameModel *self)
{
  g_return_val_if_fail (self, NULL);
  g_return_val_if_fail (self->ref_count, NULL);

  g_atomic_int_inc (&self->ref_count);

  return self;
}

/**
 * bustle_name_model_unref:
 * @self: (transfer none): A #BustleNameModel
 *
 * Decrements the reference count of @self by one, freeing the structure when
 * the reference count reaches zero.
 */
void
bustle_name_model_unref (BustleNameModel *self)
{
  g_return_if_fail (self);
  g_return_if_fail (self->ref_count);

  if (g_atomic_int_dec_and_test (&self->ref_count))
    bustle_name_model_free (self);
}

static const gchar *
bustle_name_model_resolve_name (BustleNameModel *self,
                                const gchar     *name)
{
  g_return_val_if_fail (name != NULL, NULL);

  if (name_is_unique (name))
    return name;
  else
    return g_hash_table_lookup (self->name_owners, name);
}

static void
bustle_name_model_assign_lane (BustleNameModel **self,
                               const gchar     *name)
{
  if (name == NULL)
    return;

  const gchar *owner = bustle_name_model_resolve_name (*self, name);
  if (owner == NULL)
    {
      g_warning ("%s: this is the problem: %s has no owner", G_STRFUNC, name);
    }
  else if (!g_hash_table_contains ((*self)->lanes, (gpointer) owner))
    {
      bustle_name_model_copy (self);

      g_assert ((*self)->next_lane < MAX_LANE);
      g_hash_table_insert ((*self)->lanes,
                           (gpointer) owner,
                           bustle_name_model_lane_pack ((*self)->next_lane++,
                                                        BUSTLE_NAME_MODEL_LANE_STATE_CURRENT));
    }
}

static void
bustle_name_model_add_unique (BustleNameModel **self,
                              const gchar      *name)
{
  if (!g_hash_table_contains ((*self)->owned_names, name))
    {
      bustle_name_model_copy (self);
      g_hash_table_insert ((*self)->owned_names, (gpointer) name,
                           g_hash_table_new (g_str_hash, g_str_equal));
    }
}

static void
bustle_name_model_del_unique (BustleNameModel **self,
                              const gchar      *name)
{
  GHashTableIter iter;
  gpointer key, value;

  bustle_name_model_copy (self);

  g_hash_table_iter_init (&iter, (*self)->name_owners);
  while (g_hash_table_iter_next (&iter, &key, &value))
    if (g_strcmp0 (value, name) == 0)
        g_hash_table_iter_remove (&iter);

  g_hash_table_remove ((*self)->owned_names, name);
  if (g_hash_table_lookup_extended ((*self)->lanes, name, &key, &value))
    {
      guint lane;

      bustle_name_model_lane_unpack (value, &lane, NULL);
      g_hash_table_replace ((*self)->lanes, key,
                            bustle_name_model_lane_pack (lane,
                                                         BUSTLE_NAME_MODEL_LANE_STATE_CLOSING));
      /* It will be removed when the next message is processed */
    }
}

static void
bustle_name_model_add_owner (BustleNameModel **self,
                             const gchar      *name,
                             const gchar      *unique)
{
  bustle_name_model_copy (self);

  /* Map from name to its owner */
  g_hash_table_insert ((*self)->name_owners, (gpointer) name, (gpointer) unique);

  /* Map from its owner to name */
  GHashTable *names = g_hash_table_lookup ((*self)->owned_names, unique);
  if (names == NULL)
    {
      bustle_name_model_add_unique (self, unique);
      names = g_hash_table_lookup ((*self)->owned_names, unique);
      g_assert (names != NULL);
    }
  g_hash_table_insert (names, (gpointer) name, (gpointer) name);
}

static void
bustle_name_model_del_owner (BustleNameModel **self,
                             const gchar      *name,
                             const gchar      *unique)
{
  gchar *unique_ = g_hash_table_lookup ((*self)->name_owners, name);
  if (unique_ != NULL)
    {
      bustle_name_model_copy (self);

      g_return_if_fail (g_strcmp0 (unique, unique_) == 0);
      GHashTable *names = g_hash_table_lookup ((*self)->owned_names, unique);
      g_return_if_fail (names != NULL);

      g_hash_table_remove (names, name);
      g_hash_table_remove ((*self)->name_owners, name);
    }
}

static void
bustle_name_model_update_for_name_owner_changed (BustleNameModel **self,
                                                 const gchar      *name,
                                                 const gchar      *old,
                                                 const gchar      *new)
{
  if (name[0] == ':')
    {
      if (old[0] != '\0')
        {
          /* Departing */
          /* TODO: grace */
          g_return_if_fail (new[0] == '\0');
          g_return_if_fail (g_strcmp0 (name, old) == 0);

          bustle_name_model_del_unique (self, name);
        }
      else
        {
          /* Arriving */
          g_return_if_fail (new[0] != '\0');
          g_return_if_fail (g_strcmp0 (name, new) == 0);

          bustle_name_model_add_unique (self, name);
        }
    }
  else
    {
      if (old[0] != '\0')
        {
          g_return_if_fail (name_is_unique (old));
          bustle_name_model_del_owner (self, name, old);
        }

      if (new[0] != '\0')
        {
          g_return_if_fail (name_is_unique (new));
          bustle_name_model_add_owner (self, name, new);
        }
    }
}

static void
bustle_name_model_update_for_bus_message (BustleNameModel **self,
                                          GDBusMessage     *message)
{
  GDBusMessageType message_type = g_dbus_message_get_message_type (message);
  const gchar *member = g_dbus_message_get_member (message);
  const gchar *destination = g_dbus_message_get_destination (message);
  GVariant *body = g_dbus_message_get_body (message);

  if (message_type == G_DBUS_MESSAGE_TYPE_SIGNAL &&
      0 == g_strcmp0 (member, "NameOwnerChanged") &&
      g_variant_is_of_type (body, G_VARIANT_TYPE ("(sss)")))
    {
      const gchar *name, *old, *new;
      g_variant_get (body, "(&s&s&s)",
                     &name, &old, &new);

      bustle_name_model_update_for_name_owner_changed (self, name, old, new);
    }
  else if (message_type == G_DBUS_MESSAGE_TYPE_SIGNAL &&
           0 == g_strcmp0 (member, "NameAcquired") &&
           g_variant_is_of_type (body, G_VARIANT_TYPE ("(s)")))
    {
      const gchar *name;
      g_variant_get (body, "(&s)", &name);

      bustle_name_model_update_for_name_owner_changed (self, name, "", destination);
    }
  else if (message_type == G_DBUS_MESSAGE_TYPE_SIGNAL &&
           0 == g_strcmp0 (member, "NameLost") &&
           g_variant_is_of_type (body, G_VARIANT_TYPE ("(s)")))
    {
      const gchar *name;
      g_variant_get (body, "(&s)", &name);

      bustle_name_model_update_for_name_owner_changed (self, name, destination, "");
    }
}

static void
bustle_name_model_update_for_bus_reply (BustleNameModel **self,
                                        GDBusMessage     *message,
                                        GDBusMessage     *counterpart)
{
  GDBusMessageType message_type = g_dbus_message_get_message_type (message);
  const gchar *member = g_dbus_message_get_member (counterpart);
  GVariant *message_body = g_dbus_message_get_body (message);
  GVariant *counterpart_body = g_dbus_message_get_body (counterpart);

  if (message_type == G_DBUS_MESSAGE_TYPE_METHOD_RETURN &&
      g_strcmp0 (member, "GetNameOwner") == 0 &&
      g_variant_is_of_type (message_body, G_VARIANT_TYPE ("(s)")) &&
      g_variant_is_of_type (counterpart_body, G_VARIANT_TYPE ("(s)")))
    {
      const gchar *name, *owner;

      g_variant_get (counterpart_body, "(&s)", &name);
      g_variant_get (message_body, "(&s)", &owner);

      bustle_name_model_update_for_name_owner_changed (self, name, "", owner);
    }
}

static void
bustle_name_model_advance_names (BustleNameModel **self)
{
  GHashTableIter iter;
  gpointer value;

  bustle_name_model_copy (self);

  g_hash_table_iter_init (&iter, (*self)->lanes);
  while (g_hash_table_iter_next (&iter, NULL, &value))
    {
      guint lane;
      BustleNameModelLaneState state;

      bustle_name_model_lane_unpack (value, &lane, &state);
      if (state == BUSTLE_NAME_MODEL_LANE_STATE_CLOSING)
        g_hash_table_iter_remove (&iter);
    }
}

/**
 * @self: (inout) (transfer contents):
 * @message:
 * @counterpart: (optional):
 */
void
bustle_name_model_update (BustleNameModel **self,
                          GDBusMessage    *message,
                          GDBusMessage    *counterpart)
{
  /* Slightly weird copy-on-write API. Should we make the callers see it too? */
  bustle_name_model_advance_names (self);

  const gchar *sender = g_dbus_message_get_sender (message);
  const gchar *destination = g_dbus_message_get_destination (message);
  const gchar *interface = g_dbus_message_get_interface (message);

  if (0 == g_strcmp0 (sender, DAEMON_NAME) &&
      0 == g_strcmp0 (interface, DAEMON_INTERFACE))
    {
      bustle_name_model_update_for_bus_message (self, message);
    }
  else if (counterpart != NULL &&
      g_strcmp0 (sender, DAEMON_NAME) == 0 &&
      g_strcmp0 (g_dbus_message_get_interface (counterpart), DAEMON_INTERFACE) == 0)
    {
      bustle_name_model_update_for_bus_reply (self, message, counterpart);
    }
  else if (sender != NULL &&
           !g_hash_table_contains ((*self)->owned_names, sender))
    {
      bustle_name_model_add_unique (self, sender);
    }
  /* and what about the destination? */

  bustle_name_model_assign_lane (self, sender);
  bustle_name_model_assign_lane (self, destination);
}

void
bustle_name_model_name_iter_init (BustleNameModel         *self,
                                  BustleNameModelLaneIter *iter)
{
  g_return_if_fail (self != NULL);
  g_return_if_fail (iter != NULL);

  g_hash_table_iter_init (iter, self->lanes);
}

gboolean
bustle_name_model_name_iter_next (BustleNameModelLaneIter  *iter,
                                  const gchar             **unique,
                                  guint32                  *lane)
{
  gpointer key, value;

  g_return_val_if_fail (iter != NULL, FALSE);

  if (!g_hash_table_iter_next (iter, &key, &value))
    return FALSE;

  if (unique != NULL)
    *unique = key;

  bustle_name_model_lane_unpack (value, lane, NULL);
  return TRUE;
}

gboolean
bustle_name_model_has_unique_name (BustleNameModel *self,
                                   const gchar     *unique)
{
  g_return_val_if_fail (self != NULL, FALSE);
  g_return_val_if_fail (unique != NULL, FALSE);

  return g_hash_table_contains (self->lanes, unique);
}

gboolean
bustle_name_model_get_lane (BustleNameModel *self,
                            const gchar     *name,
                            guint           *lane)
{
  g_return_val_if_fail (self != NULL, FALSE);
  g_return_val_if_fail (name != NULL, FALSE);
  g_return_val_if_fail (lane != NULL, FALSE);

  const gchar *owner = bustle_name_model_resolve_name (self, name);
  gpointer value = NULL;

  if (owner != NULL &&
      g_hash_table_lookup_extended (self->lanes, owner, NULL, &value))
    {
      bustle_name_model_lane_unpack (value, lane, NULL);
      return TRUE;
    }

  return FALSE;
}

guint32
bustle_name_model_get_first_unused_lane (BustleNameModel *self)
{
  g_return_val_if_fail (self != NULL, G_MAXUINT32);

  return self->next_lane;
}

guint
bustle_name_model_n_unique_names (BustleNameModel *self)
{
  g_return_val_if_fail (self != NULL, G_MAXUINT);
  return g_hash_table_size (self->owned_names);
}
