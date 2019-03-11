#pragma once

#include <gtk/gtk.h>

G_BEGIN_DECLS

#define BUSTLE_TYPE_CELL_RENDERER_CHART (bustle_cell_renderer_chart_get_type())

G_DECLARE_FINAL_TYPE (BustleCellRendererChart, bustle_cell_renderer_chart, BUSTLE, CELL_RENDERER_CHART, GtkCellRenderer)

GtkCellRenderer *bustle_cell_renderer_chart_new (void);

G_END_DECLS
