#include <gtk/gtk.h>

#include "bustle-window.h"

static void
open_cb (GApplication *application,
         GFile       **files,
         gint          n_files,
         gchar        *hint,
         gpointer      user_data)
{
  for (int i = 0; i < n_files; i++)
    {
      BustleWindow *window = bustle_window_new (GTK_APPLICATION (application));
      bustle_window_load_file (window, files[i]);
    }
}

static void
activate_cb (GApplication *application,
             gpointer user_data)
{
  bustle_window_new (GTK_APPLICATION (application));
}

gint
main (gint   argc,
      gchar *argv[])
{
  g_autoptr(GtkApplication) app = gtk_application_new ("org.freedesktop.Bustle",
                                                       G_APPLICATION_HANDLES_OPEN);

  g_signal_connect (app, "activate", G_CALLBACK (activate_cb), NULL);
  g_signal_connect (app, "open", G_CALLBACK (open_cb), NULL);

  return g_application_run (G_APPLICATION (app), argc, argv);
}
