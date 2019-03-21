#include "bustle-cell-renderer-chart.h"
#include "bustle-name-model.h"

#define LINE_WIDTH 2
#define LANE_WIDTH 24
#define SIGNAL_RADIUS 4

typedef struct
{
  guint8 r, g, b;
} Rgb;

static const Rgb palette[] = {
    {196, 160, 0},
    {78, 154, 6},
    {206, 92, 0},
    {32, 74, 135},
    {108, 53, 102},
    {164, 0, 0},

    {138, 226, 52},
    {252, 175, 62},
    {114, 159, 207},
    {252, 233, 79},
    {136, 138, 133},
    {173, 127, 168},
    {233, 185, 110},
    {239, 41, 41}
};

#define PALETTE_SIZE (G_N_ELEMENTS (palette))

struct _BustleCellRendererChart
{
  GtkCellRenderer parent_instance;

  GDBusMessage *message;
  BustleNameModel *name_model_prev;
  BustleNameModel *name_model;
  BustleNameModel *name_model_next;
};

G_DEFINE_TYPE (BustleCellRendererChart, bustle_cell_renderer_chart, GTK_TYPE_CELL_RENDERER)

typedef enum {
  PROP_DBUS_MESSAGE = 1,
  PROP_NAME_MODEL_PREV,
  PROP_NAME_MODEL,
  PROP_NAME_MODEL_NEXT,
  N_PROPS
} BustleCellRendererChartProperty;

static GParamSpec *properties [N_PROPS];

GtkCellRenderer *
bustle_cell_renderer_chart_new (void)
{
  return g_object_new (BUSTLE_TYPE_CELL_RENDERER_CHART, NULL);
}

static void
bustle_cell_renderer_chart_finalize (GObject *object)
{
  BustleCellRendererChart *self = (BustleCellRendererChart *)object;

  g_clear_object (&self->message);
  g_clear_pointer (&self->name_model_prev, bustle_name_model_unref);
  g_clear_pointer (&self->name_model, bustle_name_model_unref);
  g_clear_pointer (&self->name_model_next, bustle_name_model_unref);

  G_OBJECT_CLASS (bustle_cell_renderer_chart_parent_class)->finalize (object);
}

static void
bustle_cell_renderer_chart_get_property (GObject    *object,
                                         guint       prop_id,
                                         GValue     *value,
                                         GParamSpec *pspec)
{
  BustleCellRendererChart *self = BUSTLE_CELL_RENDERER_CHART (object);

  switch ((BustleCellRendererChartProperty) prop_id)
    {
    case PROP_DBUS_MESSAGE:
      g_value_set_object (value, self->message);
      break;

    case PROP_NAME_MODEL_PREV:
      g_value_set_boxed (value, self->name_model_prev);
      break;

    case PROP_NAME_MODEL:
      g_value_set_boxed (value, self->name_model);
      break;

    case PROP_NAME_MODEL_NEXT:
      g_value_set_boxed (value, self->name_model_next);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
    }
}

static void
bustle_cell_renderer_chart_set_property (GObject      *object,
                                         guint         prop_id,
                                         const GValue *value,
                                         GParamSpec   *pspec)
{
  BustleCellRendererChart *self = BUSTLE_CELL_RENDERER_CHART (object);

  switch ((BustleCellRendererChartProperty) prop_id)
    {
    case PROP_DBUS_MESSAGE:
      g_set_object (&self->message, g_value_dup_object (value));
      break;

    case PROP_NAME_MODEL_PREV:
      g_clear_pointer (&self->name_model_prev, bustle_name_model_unref);
      self->name_model_prev = g_value_dup_boxed (value);
      break;

    case PROP_NAME_MODEL:
      g_clear_pointer (&self->name_model, bustle_name_model_unref);
      self->name_model = g_value_dup_boxed (value);
      g_assert (self->name_model != NULL);
      break;

    case PROP_NAME_MODEL_NEXT:
      g_clear_pointer (&self->name_model_next, bustle_name_model_unref);
      self->name_model_next = g_value_dup_boxed (value);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
    }
}


static void
bustle_cell_renderer_chart_get_size (GtkCellRenderer    *cell,
                                     GtkWidget          *widget,
                                     const GdkRectangle *cell_area,
                                     gint               *x_offset,
                                     gint               *y_offset,
                                     gint               *width,
                                     gint               *height)
{
  BustleCellRendererChart *self = BUSTLE_CELL_RENDERER_CHART (cell);

  *width = LANE_WIDTH * (bustle_name_model_get_first_unused_lane (self->name_model) + 1);
  *height = 25;
}

static inline void
bustle_cell_renderer_chart_get_lane_centre (const GdkRectangle *cell_area,
                                            const GdkRectangle *background_area,
                                            guint               lane,
                                            double             *x,
                                            double             *y)
{
  g_assert (x != NULL);
  *x = cell_area->x + (lane * LANE_WIDTH) + (LANE_WIDTH / 2);

  if (y != NULL)
    *y = background_area->y + (background_area->height / 2) - LINE_WIDTH;
}

static void
bustle_cell_renderer_chart_draw_arrow (cairo_t            *cr,
                                       const GdkRectangle *cell_area,
                                       const GdkRectangle *background_area,
                                       double              from_x,
                                       double              to_x,
                                       double              y,
                                       GDBusMessageType    message_type)
{
  static double return_dashes[] = { LINE_WIDTH };
  static double error_dashes[] = { LANE_WIDTH / 3, LINE_WIDTH };
  double direction = from_x < to_x ? -1. : 1.;

  cairo_save (cr);

  if (message_type == G_DBUS_MESSAGE_TYPE_SIGNAL)
    cairo_move_to (cr, from_x - (direction * SIGNAL_RADIUS), y);
  else
    cairo_move_to (cr, from_x, y);

  cairo_line_to (cr, to_x, y);
  cairo_set_line_width (cr, LINE_WIDTH);
  switch (message_type)
    {
    case G_DBUS_MESSAGE_TYPE_METHOD_CALL:
    case G_DBUS_MESSAGE_TYPE_SIGNAL:
      cairo_set_source_rgb (cr, 0, 0, 0);
      cairo_set_dash (cr, NULL, 0, 0);
      break;

    case G_DBUS_MESSAGE_TYPE_METHOD_RETURN:
      cairo_set_source_rgb (cr, 0, 0, 0);
      cairo_set_dash (cr, return_dashes, G_N_ELEMENTS (return_dashes), 0);
      break;

    case G_DBUS_MESSAGE_TYPE_ERROR:
      cairo_set_source_rgb (cr, 239 / 255., 41 / 255., 41 / 255.);
      cairo_set_dash (cr, error_dashes, G_N_ELEMENTS (error_dashes), 0);
      break;

    default:
      g_assert_not_reached ();
    }
  cairo_stroke (cr);

  double q = LANE_WIDTH / 4;

  cairo_move_to (cr, to_x + (direction * q), y - q);
  cairo_line_to (cr, to_x, y);
  cairo_line_to (cr, to_x + (direction * q), y + q);
  cairo_set_dash (cr, NULL, 0, 0);
  cairo_stroke (cr);

  cairo_restore (cr);
}

static void
bustle_cell_renderer_chart_render (GtkCellRenderer      *cell,
                                   cairo_t              *cr,
                                   GtkWidget            *widget,
                                   const GdkRectangle   *background_area,
                                   const GdkRectangle   *cell_area,
                                   GtkCellRendererState  flags)
{
  BustleCellRendererChart *self = BUSTLE_CELL_RENDERER_CHART (cell);
  BustleNameModelLaneIter iter;
  const gchar *name;
  guint32 lane;

  bustle_name_model_name_iter_init (self->name_model, &iter);
  while (bustle_name_model_name_iter_next (&iter, &name, &lane))
    {
      gdouble x, y1, y2;

      bustle_cell_renderer_chart_get_lane_centre (cell_area, background_area, lane, &x, NULL);

      const Rgb *color = &palette[lane % PALETTE_SIZE];
      cairo_set_line_width (cr, LINE_WIDTH);
      cairo_set_source_rgb (cr, color->r / 255., color->g / 255., color->b / 255.);

      if (self->name_model_prev == NULL ||
          !bustle_name_model_has_unique_name (self->name_model_prev, name))
        {
          /* arriving */
          y1 = background_area->y + background_area->height * 1 / 4;
          cairo_arc (cr, x, y1, LINE_WIDTH, 0., 2 * G_PI);
          cairo_stroke_preserve (cr);
          cairo_fill (cr);
        }
      else
        {
          y1 = background_area->y;
        }

      if (self->name_model_next == NULL ||
          !bustle_name_model_has_unique_name (self->name_model_next, name))
        {
          /* leaving */
          y2 = background_area->y + background_area->height * 3 / 4;
          cairo_arc (cr, x, y2, LINE_WIDTH, 0., 2 * G_PI);
          cairo_stroke_preserve (cr);
          cairo_fill (cr);
        }
      else
        {
          y2 = background_area->y + background_area->height;
        }

      cairo_move_to (cr, x, y1);
      cairo_line_to (cr, x, y2);
      cairo_stroke (cr);
    }

  const gchar *sender = g_dbus_message_get_sender (self->message);
  const gchar *destination = g_dbus_message_get_destination (self->message);
  GDBusMessageType message_type = g_dbus_message_get_message_type (self->message);
  guint32 from_lane, to_lane;
  double from_x, from_y, to_x;

  if (sender != NULL)
    {
      if (!bustle_name_model_get_lane (self->name_model, sender, &from_lane))
        {
          g_message ("%s: no lane for sender %s", G_STRFUNC, sender);
          return;
        }
    }
  else
    {
      /* Nothing better to do than make the message emerge from the æther. */
      from_lane = bustle_name_model_get_first_unused_lane (self->name_model);
    }

  bustle_cell_renderer_chart_get_lane_centre (cell_area, background_area, from_lane, &from_x, &from_y);

  if (message_type == G_DBUS_MESSAGE_TYPE_SIGNAL)
    {
      cairo_arc (cr, from_x, from_y, SIGNAL_RADIUS, 0., 2 * G_PI);
      cairo_set_line_width (cr, LINE_WIDTH);
      cairo_set_source_rgb (cr, 0, 0, 0);
      cairo_stroke (cr);
    }

  if (destination != NULL)
    {
      if (!bustle_name_model_get_lane (self->name_model, destination, &to_lane))
        {
          g_message ("%s: no lane for destination %s", G_STRFUNC, destination);
          to_lane = bustle_name_model_get_first_unused_lane (self->name_model);
        }

      bustle_cell_renderer_chart_get_lane_centre (cell_area, background_area, to_lane, &to_x, NULL);
      bustle_cell_renderer_chart_draw_arrow (cr, cell_area, background_area,
                                             from_x, to_x, from_y, message_type);
    }
  else
    {
      bustle_cell_renderer_chart_draw_arrow (cr, cell_area, background_area,
                                             from_x, from_x - (LANE_WIDTH / 2),
                                             from_y, message_type);
      bustle_cell_renderer_chart_draw_arrow (cr, cell_area, background_area,
                                             from_x, from_x + (LANE_WIDTH / 2),
                                             from_y, message_type);
    }

  /* TODO: it's hard to draw an arc. We can probably work out how many rows
   * there are between this one and the other one, and assume they are constant
   * height, and from that determine the cell area of the target (and the
   * coordinate of the other end of the line). Or change the design so only
   * local data is needed? The trouble is that we can't just assume it's a
   * straight line because if the name is not known, the bus daemon will reply,
   * guaranteed to be a different column. (Even if we could reliably line up
   * call and return.) Is this just totally intractable?
   *
   * We also need non-local information in the form of "all calls that are
   * outstanding" because we need to draw all stripes of the curve. Cool!
   */
}

static void
bustle_cell_renderer_chart_class_init (BustleCellRendererChartClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
  GtkCellRendererClass *renderer_class = GTK_CELL_RENDERER_CLASS (klass);

  object_class->finalize = bustle_cell_renderer_chart_finalize;
  object_class->get_property = bustle_cell_renderer_chart_get_property;
  object_class->set_property = bustle_cell_renderer_chart_set_property;

  renderer_class->get_size = bustle_cell_renderer_chart_get_size;
  renderer_class->render = bustle_cell_renderer_chart_render;

  properties [PROP_DBUS_MESSAGE] =
    g_param_spec_object ("dbus-message",
                         "GDBusMessage",
                         "DBus message to render",
                         G_TYPE_DBUS_MESSAGE,
                         (G_PARAM_READWRITE |
                          G_PARAM_STATIC_STRINGS));
  g_object_class_install_property (object_class, PROP_DBUS_MESSAGE,
                                   properties [PROP_DBUS_MESSAGE]);

  properties [PROP_NAME_MODEL_PREV] =
    g_param_spec_boxed ("name-model-prev",
                        "NameModel",
                        "Name model on the previous visible row, or NULL if "
                        "this is the first visible row",
                        BUSTLE_TYPE_NAME_MODEL,
                        (G_PARAM_READWRITE |
                         G_PARAM_STATIC_STRINGS));
  g_object_class_install_property (object_class, PROP_NAME_MODEL_PREV,
                                   properties [PROP_NAME_MODEL_PREV]);

  properties [PROP_NAME_MODEL] =
    g_param_spec_boxed ("name-model",
                        "NameModel",
                        "Name model on this row",
                        BUSTLE_TYPE_NAME_MODEL,
                        (G_PARAM_READWRITE |
                         G_PARAM_STATIC_STRINGS));
  g_object_class_install_property (object_class, PROP_NAME_MODEL,
                                   properties [PROP_NAME_MODEL]);


  properties [PROP_NAME_MODEL_NEXT] =
    g_param_spec_boxed ("name-model-next",
                        "NameModel",
                        "Name model on the next visible row, or NULL if "
                        "this is the last visible row",
                        BUSTLE_TYPE_NAME_MODEL,
                        (G_PARAM_READWRITE |
                         G_PARAM_STATIC_STRINGS));
  g_object_class_install_property (object_class, PROP_NAME_MODEL_NEXT,
                                   properties [PROP_NAME_MODEL_NEXT]);
}

static void
bustle_cell_renderer_chart_init (BustleCellRendererChart *self)
{
  self->name_model = bustle_name_model_new ();
}
