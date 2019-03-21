gnome = import('gnome')

glib_version = '>= 2.54'

deps = [
  dependency('glib-2.0', version: glib_version),
  dependency('gobject-2.0', version: glib_version),
  dependency('gio-2.0', version: glib_version),
  dependency('gio-unix-2.0', version: glib_version),
  dependency('gtk+-3.0'),
  dependency('libpcap', version: '>= 1.9.0'),
]

libbustle_pcap_sources = [
  'config.h',
  'pcap-monitor.c',
  'pcap-monitor.h',
]

libbustle_pcap = static_library(
  'bustle-pcap',
  libbustle_pcap_sources,
  dependencies: deps,
)

bustle_pcap = executable(
  'bustle-pcap',
  ['bustle-pcap.c', version_h],
  link_with: [libbustle_pcap],
  dependencies: deps,
  install: true,
)

bustle_resources = gnome.compile_resources(
  'bustle-resources',
  'bustle.gresource.xml',
  c_name: 'bustle'
)

bustle_sources = [
  'bustle-cell-renderer-chart.c',
  'bustle-cell-renderer-chart.h',
  'bustle-model.c',
  'bustle-model.h',
  'bustle-name-model.c',
  'bustle-name-model.h',
  'bustle-window.c',
  'bustle-window.h',
  'bustle-viewer.c',
  'bustle-pcap-reader.c',
  'bustle-pcap-reader.h',
  bustle_resources,
]

bustle = executable(
  'bustle',
  bustle_sources,
  link_with: [libbustle_pcap],
  dependencies: deps,
  install: true,
)
