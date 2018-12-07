Bustle draws sequence diagrams of D-Bus activity, showing signal
emissions, method calls and their corresponding returns, with timestamps
for each individual event and the duration of each method call. This can
help you check for unwanted D-Bus traffic, and pinpoint why your
D-Bus-based application isn't performing as well as you like. It also
provides statistics like signal frequencies and average method call
times.

[![pipeline status](https://gitlab.freedesktop.org/bustle/bustle/badges/master/pipeline.svg)](https://gitlab.freedesktop.org/bustle/bustle/commits/master)

<a href='https://flathub.org/apps/details/org.freedesktop.Bustle'><img width='240' alt='Download on Flathub' src='https://flathub.org/assets/badges/flathub-badge-en.png'/></a>


Using Bustle
============

Run it:

    bustle

Now click **File → New…** to start recording session bus traffic. When you're
done, click **Stop**, and explore the log.

If you want to record traffic without running the UI (maybe on an embedded
platform which doesn't have Gtk+ and/or a Haskell compiler), you can use the
stand-alone logger:

    bustle-pcap logfile.bustle

You can then open `logfile.bustle` in Bustle.

You can also get some ASCII-art
version of the statistics shown in the UI:

    bustle --count logfile.bustle
    bustle --time logfile.bustle
