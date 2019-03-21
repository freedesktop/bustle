#include "bustle-model.h"
#include "bustle-name-model.h"

typedef struct _PendingKey {
  /* Borrowed from message */
  const char *sender;
  guint32 serial;
} PendingKey;

static PendingKey *
pending_key_new (const char *sender,
                 guint32     serial)
{
  PendingKey *key = g_slice_new (PendingKey);
  key->sender = sender;
  key->serial = serial;
  return key;
}

static PendingKey *
pending_key_for_call (GDBusMessage *message)
{
  return pending_key_new (g_dbus_message_get_sender (message),
                          g_dbus_message_get_serial (message));
}

static PendingKey *
pending_key_for_reply (GDBusMessage *message)
{
  return pending_key_new (g_dbus_message_get_destination (message),
                          g_dbus_message_get_reply_serial (message));
}

static void
pending_key_free (PendingKey *key)
{
  g_slice_free (PendingKey, key);
}

G_DEFINE_AUTOPTR_CLEANUP_FUNC (PendingKey, pending_key_free);

static guint
pending_key_hash (gconstpointer key_)
{
  const PendingKey *key = key_;

  return 31 * g_str_hash (key->sender) + g_int_hash (&key->serial);
}

static gboolean
pending_key_equal (gconstpointer a_,
                   gconstpointer b_)
{
  const PendingKey *a = a_;
  const PendingKey *b = b_;

  return (a->serial == b->serial) && (0 == g_strcmp0 (a->sender, b->sender));
}

typedef struct _PendingValue {
  /* owned */
  GtkTreeRowReference *ref;
  /* Set to TRUE once we've seen a reply for this call */
  gboolean seen;
} PendingValue;

static PendingValue *
pending_value_new (GtkTreeRowReference *ref)
{
  PendingValue *value = g_slice_new (PendingValue);
  value->ref = ref;
  return value;
}

static void
pending_value_free (PendingValue *value)
{
  g_clear_pointer (&value->ref, gtk_tree_row_reference_free);
  g_slice_free (PendingValue, value);
}

G_DEFINE_AUTOPTR_CLEANUP_FUNC (PendingValue, pending_value_free);

struct _BustleModel
{
  GObject parent_instance;

  GtkTreeModel *model;
  /* owned PendingKey => owned PendingValue */
  GHashTable *pending;

  BustleNameModel *name_model;
};

G_DEFINE_TYPE (BustleModel, bustle_model, G_TYPE_OBJECT)

BustleModel *
bustle_model_new (void)
{
  return g_object_new (BUSTLE_TYPE_MODEL, NULL);
}

static void
bustle_model_dispose (GObject *object)
{
  BustleModel *self = (BustleModel *)object;

  g_clear_pointer (&self->pending, g_hash_table_destroy);
  g_clear_object (&self->model);

  G_OBJECT_CLASS (bustle_model_parent_class)->dispose (object);
}

static void
bustle_model_class_init (BustleModelClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->dispose = bustle_model_dispose;
}

static void
bustle_model_init (BustleModel *self)
{
  self->model = GTK_TREE_MODEL (gtk_list_store_new (6,
                                                    G_TYPE_INT64,
                                                    G_TYPE_DBUS_MESSAGE,
                                                    GTK_TYPE_TREE_ROW_REFERENCE,
                                                    G_TYPE_BOOLEAN,
                                                    BUSTLE_TYPE_NAME_MODEL,
                                                    G_TYPE_PTR_ARRAY));
  self->pending = g_hash_table_new_full (pending_key_hash,
                                         pending_key_equal,
                                         (GDestroyNotify) pending_key_free,
                                         (GDestroyNotify) pending_value_free);

  self->name_model = bustle_name_model_new ();
}

/**
 * bustle_model_get_tree_model:
 *
 * Returns: (transfer none): a model with columns indexed by %BustleModelColumn.
 */
GtkTreeModel *
bustle_model_get_tree_model (BustleModel  *self)
{
  g_return_val_if_fail (BUSTLE_IS_MODEL (self), NULL);

  return self->model;
}

static void
add_term (GPtrArray   *terms,
          const gchar *string)
{
  if (string != NULL)
    {
      g_auto(GStrv) new_terms = g_str_tokenize_and_fold (string, NULL, NULL);

      for (gchar **s = new_terms; *s != NULL; s++)
        g_ptr_array_add (terms, g_ref_string_new_intern (*s));
    }
}

static void
add_terms (GPtrArray    *terms,
           GDBusMessage *message)
{
  g_assert (message != NULL);

  add_term (terms, g_dbus_message_get_sender (message));
  add_term (terms, g_dbus_message_get_destination (message));
  add_term (terms, g_dbus_message_get_path (message));
  add_term (terms, g_dbus_message_get_interface (message));
  add_term (terms, g_dbus_message_get_member (message));
  add_term (terms, g_dbus_message_get_error_name (message));
  /* TODO: body, too? */
}

/**
 * bustle_model_add_message:
 * @self:
 * @timestamp_usec:
 * @message: (transfer none):
 *
 *
 * Appends a message to the model.
 */
void
bustle_model_add_message (BustleModel  *self,
                          gint64        timestamp_usec,
                          GDBusMessage *message)
{
  g_return_if_fail (BUSTLE_IS_MODEL (self));
  g_return_if_fail (G_IS_DBUS_MESSAGE (message));

  g_autoptr(PendingKey) key = NULL;
  PendingValue *value = NULL;
  g_autoptr(GtkTreePath) self_path = NULL;
  g_autoptr(GtkTreePath) existing_path = NULL;
  g_autoptr(GtkTreeRowReference) self_ref = NULL;
  g_autoptr(GtkTreeRowReference) existing_ref = NULL;
  GtkTreeIter self_iter;
  GtkTreeIter existing_iter;
  g_autoptr(GDBusMessage) counterpart = NULL;
  g_autoptr(GPtrArray) terms = g_ptr_array_new_with_free_func ((GDestroyNotify) g_ref_string_release);

  add_terms (terms, message);

  switch (g_dbus_message_get_message_type (message))
    {
    case G_DBUS_MESSAGE_TYPE_SIGNAL:
      /* Let's get the easy one out of the way */
      g_ptr_array_add (terms, NULL);
      gtk_list_store_insert_with_values (GTK_LIST_STORE (self->model),
                                         &self_iter, -1,
                                         BUSTLE_MODEL_COLUMN_TIMESTAMP_USEC, timestamp_usec,
                                         BUSTLE_MODEL_COLUMN_DBUS_MESSAGE, message,
                                         BUSTLE_MODEL_COLUMN_SEARCH_TOKENS, terms,
                                         -1);
      break;

    case G_DBUS_MESSAGE_TYPE_METHOD_CALL:
      g_ptr_array_add (terms, NULL);
      gtk_list_store_insert_with_values (GTK_LIST_STORE (self->model),
                                         &self_iter, -1,
                                         BUSTLE_MODEL_COLUMN_TIMESTAMP_USEC, timestamp_usec,
                                         BUSTLE_MODEL_COLUMN_DBUS_MESSAGE, message,
                                         BUSTLE_MODEL_COLUMN_SEARCH_TOKENS, terms,
                                         -1);
      key = pending_key_for_call (message);
      self_path = gtk_tree_model_get_path (self->model, &self_iter);
      self_ref = gtk_tree_row_reference_new (self->model, self_path);
      if (!g_hash_table_replace (self->pending,
                                 g_steal_pointer (&key),
                                 pending_value_new (g_steal_pointer (&self_ref))))
        g_message ("oh no, %s %d was already pending",
                   g_dbus_message_get_sender (message),
                   g_dbus_message_get_serial (message));
      break;

    case G_DBUS_MESSAGE_TYPE_METHOD_RETURN:
    case G_DBUS_MESSAGE_TYPE_ERROR:
      key = pending_key_for_reply (message);
      value = g_hash_table_lookup (self->pending, key);

      if (value != NULL)
        {
          g_assert (value->ref != NULL);
          existing_ref = gtk_tree_row_reference_copy (value->ref);
          existing_path = gtk_tree_row_reference_get_path (value->ref);

          gtk_tree_model_get_iter (self->model, &existing_iter, existing_path);
          gtk_tree_model_get (self->model, &existing_iter,
                              BUSTLE_MODEL_COLUMN_DBUS_MESSAGE, &counterpart,
                              -1);

          add_terms (terms, counterpart);
        }

      g_ptr_array_add (terms, NULL);
      gtk_list_store_insert_with_values (GTK_LIST_STORE (self->model),
                                         &self_iter, -1,
                                         BUSTLE_MODEL_COLUMN_TIMESTAMP_USEC, timestamp_usec,
                                         BUSTLE_MODEL_COLUMN_DBUS_MESSAGE, message,
                                         BUSTLE_MODEL_COLUMN_COUNTERPART, g_steal_pointer (&existing_ref),
                                         BUSTLE_MODEL_COLUMN_DUPLICATE_REPLY, value && value->seen,
                                         BUSTLE_MODEL_COLUMN_SEARCH_TOKENS, terms,
                                         -1);
      if (value != NULL && !value->seen)
        {
          self_path = gtk_tree_model_get_path (self->model, &self_iter);
          self_ref = gtk_tree_row_reference_new (self->model, self_path);

          gtk_list_store_set (GTK_LIST_STORE (self->model),
                              &existing_iter,
                              BUSTLE_MODEL_COLUMN_COUNTERPART, g_steal_pointer (&self_ref),
                              -1);
          value->seen = TRUE;
        }
      else
        {
          g_clear_object (&counterpart);
        }
      break;

    default:
      break;
    }

  bustle_name_model_update (&self->name_model, message, counterpart);

  /* TODO: perform exactly 1 insert and at most 1 update... */
  gtk_list_store_set (GTK_LIST_STORE (self->model),
                      &self_iter,
                      BUSTLE_MODEL_COLUMN_NAME_MODEL, self->name_model,
                      -1);
}

struct _BustleModelSearchData {
  const gchar *key;
  gchar **tokens;
};

BustleModelSearchData *
bustle_model_search_data_new (void)
{
  BustleModelSearchData *data = g_new0 (BustleModelSearchData, 1);

  return data;
}

void
bustle_model_search_data_free (BustleModelSearchData *data)
{
  g_clear_pointer (&data->tokens, g_strfreev);
  g_free (data);
}

gboolean
bustle_model_search_equal_func (GtkTreeModel *model,
                                gint          column,
                                const gchar  *key,
                                GtkTreeIter  *iter,
                                gpointer      search_data)
{
  BustleModelSearchData *data = search_data;
  g_autoptr(GPtrArray) hit_tokens = NULL;

  if (key != data->key)
    {
      g_clear_pointer (&data->tokens, g_strfreev);
      data->tokens = g_str_tokenize_and_fold (key, NULL, NULL);
    }

  gtk_tree_model_get (model, iter,
                      BUSTLE_MODEL_COLUMN_SEARCH_TOKENS, &hit_tokens,
                      -1);
  for (gsize i = 0; data->tokens[i] != NULL; i++)
    {
      const gchar *search_token = data->tokens[i];
      gsize j = 0;

      while (hit_tokens->pdata[j] != NULL &&
             !g_str_has_prefix (hit_tokens->pdata[j], search_token))
        j++;

      if (hit_tokens->pdata[j] == NULL)
        return TRUE; /* no match */
    }

  return FALSE; /* match */
}
