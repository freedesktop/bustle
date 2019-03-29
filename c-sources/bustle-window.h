#pragma once

#include <gtk/gtk.h>

G_BEGIN_DECLS

#define BUSTLE_TYPE_WINDOW (bustle_window_get_type())

G_DECLARE_FINAL_TYPE (BustleWindow, bustle_window, BUSTLE, WINDOW, GtkApplicationWindow)

BustleWindow *bustle_window_new       (GtkApplication *application);
void          bustle_window_load_file (BustleWindow   *self,
                                       GFile          *file);

G_END_DECLS
