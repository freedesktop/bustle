Installing from Flathub
=======================

If you don't want to modify Bustle, by far the easiest way to install it is to
[get it from Flathub](https://gitlab.freedesktop.org/bustle/bustle).

<a href='https://flathub.org/apps/details/org.freedesktop.Bustle'><img width='240' alt='Download on Flathub' src='https://flathub.org/assets/badges/flathub-badge-en.png'/></a>

Building from source
====================

I recommend using Stack; see the instructions in
[CONTRIBUTING.md](./CONTRIBUTING.md). You can also build a Git checkout using
Flatpak:

```
flatpak-builder --install --user --force-clean app flatpak/org.freedesktop.Bustle.yaml
```

On exotic platforms with no Haskell toolchain
=============================================

If you're working on an embedded platform, you may have D-Bus but no Haskell
toolchain, and you may not want to bootstrap everything just to run Bustle on
the target device. That's fine: Bustle was originally written for exactly this
situation!

First, install Bustle on your (x86_64) development system as above. You then
have two options to monitor D-Bus traffic on the target device:

1. On the target device, run `dbus-monitor --pcap --session >session.pcap`, and
   hit `Ctrl+C` when you're done. Then copy `session.pcap` to your development
   system, and open it in Bustle.
2. On the target device, arrange for D-Bus to be accessible via TCP or via some
   kind of socket forwarding. On your development system, run Bustle, choose
   _Record → Record Address_, and enter the remote address in the same form
   accepted by `dbus-monitor`.
