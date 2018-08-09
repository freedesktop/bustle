CFLAGS = -g -O2 -Wall -Wunused -Waddress
DBUS_FLAGS = $(shell pkg-config --cflags --libs dbus-1)
GIO_FLAGS := $(shell pkg-config --cflags --libs 'glib-2.0 >= 2.26' gio-2.0 gio-unix-2.0)
PCAP_FLAGS := $(shell pcap-config --cflags pcap-config --libs)
DESTDIR =
PREFIX = /usr/local
BINDIR = $(DESTDIR)$(PREFIX)/bin
DATADIR = $(DESTDIR)$(PREFIX)/share
MAN1DIR = $(DATADIR)/man/man1

BINARIES = \
	dist/build/bustle-pcap \
	$(NULL)

MANPAGE = bustle-pcap.1
DESKTOP_FILE = org.freedesktop.Bustle.desktop
APPDATA_FILE = org.freedesktop.Bustle.appdata.xml
ICON_SIZES = 16x16 22x22 32x32 48x48 256x256
SCALABLE_ICONS = \
	data/icons/hicolor/scalable/apps/org.freedesktop.Bustle.svg \
	data/icons/hicolor/scalable/apps/org.freedesktop.Bustle-symbolic.svg \
	$(NULL)
ICONS = \
	$(SCALABLE_ICONS) \
	$(foreach size,$(ICON_SIZES),data/icons/hicolor/$(size)/apps/org.freedesktop.Bustle.png) \

all: $(BINARIES) $(MANPAGE) $(DESKTOP_FILE) $(APPDATA_FILE) $(ICONS)

BUSTLE_PCAP_SOURCES = c-sources/pcap-monitor.c c-sources/bustle-pcap.c
BUSTLE_PCAP_GENERATED_HEADERS = dist/build/autogen/version.h
BUSTLE_PCAP_HEADERS = c-sources/pcap-monitor.h $(BUSTLE_PCAP_GENERATED_HEADERS)

bustle-pcap.1: dist/build/bustle-pcap
	help2man --output=$@ --no-info --name='Generate D-Bus logs for bustle' $<

org.freedesktop.Bustle.desktop: data/org.freedesktop.Bustle.desktop.in
	msgfmt --desktop -d po --template $< -o $@

org.freedesktop.Bustle.appdata.xml: data/org.freedesktop.Bustle.appdata.xml.in
	msgfmt --xml -d po --template $< -o $@

# https://github.com/flathub/flathub/wiki/Review-Guidelines
validate-metadata: org.freedesktop.Bustle.desktop org.freedesktop.Bustle.appdata.xml
	desktop-file-validate org.freedesktop.Bustle.desktop
	appstream-util validate-relax org.freedesktop.Bustle.appdata.xml
	# This is only a SHOULD. Screenshots currently violate it because they
	# are hidpi.
	appstream-util validate org.freedesktop.Bustle.appdata.xml || true

dist/build/bustle-pcap: $(BUSTLE_PCAP_SOURCES) $(BUSTLE_PCAP_HEADERS)
	@mkdir -p dist/build
	$(CC) -Idist/build/autogen $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) \
		-o $@ $(BUSTLE_PCAP_SOURCES) \
		$(GIO_FLAGS) $(PCAP_FLAGS)

dist/build/autogen/version.txt: bustle.cabal
	@mkdir -p `dirname $@`
	perl -nle 'm/^Version:\s+(.*)$$/ and print $$1' \
		$< > $@

dist/build/autogen/version.h: dist/build/autogen/version.txt
	echo '#define BUSTLE_VERSION "'`cat $<`'"' > $@

install: all
	mkdir -p $(BINDIR)
	cp $(BINARIES) $(BINDIR)
	-mkdir -p $(MAN1DIR)
	-cp $(MANPAGE) $(MAN1DIR)
	mkdir -p $(DATADIR)/applications
	cp $(DESKTOP_FILE) $(DATADIR)/applications
	mkdir -p $(DATADIR)/appdata
	cp $(APPDATA_FILE) $(DATADIR)/appdata
	$(foreach size,$(ICON_SIZES),mkdir -p $(DATADIR)/icons/hicolor/$(size)/apps; )
	$(foreach size,$(ICON_SIZES),cp data/icons/hicolor/$(size)/apps/org.freedesktop.Bustle.png $(DATADIR)/icons/hicolor/$(size)/apps; )
	mkdir -p $(DATADIR)/icons/hicolor/scalable/apps
	cp $(SCALABLE_ICONS) $(DATADIR)/icons/hicolor/scalable/apps
	$(MAKE) update-icon-cache

uninstall:
	rm -f $(BINDIR)/$(notdir $(BINARIES))
	rm -f $(MAN1DIR)/$(MANPAGE)
	rm -f $(DATADIR)/applications/$(DESKTOP_FILE)
	rm -f $(DATADIR)/appdata/$(APPDATA_FILE)
	$(foreach size,$(ICON_SIZES),rm -f $(DATADIR)/icons/hicolor/$(size)/apps/org.freedesktop.Bustle.png)
	rm -f $(DATADIR)/icons/hicolor/scalable/apps/org.freedesktop.Bustle-symbolic.svg
	$(MAKE) update-icon-cache

clean:
	rm -f $(BINARIES) $(MANPAGE) $(BUSTLE_PCAP_GENERATED_HEADERS) $(DESKTOP_FILE) $(APPDATA_FILE)

# Icon cache stuff
gtk_update_icon_cache = gtk-update-icon-cache -f -t $(DATADIR)/icons/hicolor

update-icon-cache:
	@-if test -z "$(DESTDIR)"; then \
		echo "Updating GTK+ icon cache."; \
		$(gtk_update_icon_cache); \
	else \
		echo "*** Icon cache not updated.  After (un)install, run this:"; \
		echo "***   $(gtk_update_icon_cache)"; \
	fi

# Flatpak stuff
org.freedesktop.Bustle.flatpak: flatpak/org.freedesktop.Bustle.json
	rm -rf _build
	flatpak-builder --repo=repo -v _build $<
	flatpak build-bundle repo org.freedesktop.Bustle.flatpak org.freedesktop.Bustle

# Maintainer stuff
maintainer-update-messages-pot:
	find Bustle -name '*.hs' -print0 | xargs -0 stack exec -- hgettext -k __ -o po/messages.pot
	xgettext data/bustle.ui data/org.freedesktop.Bustle.desktop.in \
		data/org.freedesktop.Bustle.appdata.xml.in --join-existing -o po/messages.pot

maintainer-make-release: bustle.cabal dist/build/autogen/version.txt
	stack sdist --test-tarball
	git tag -s -m 'Bustle '`cat dist/build/autogen/version.txt` \
		bustle-`cat dist/build/autogen/version.txt`
	gpg --detach-sign --armor dist/bustle-`cat dist/build/autogen/version.txt`.tar.gz
