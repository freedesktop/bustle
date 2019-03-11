project('bustle', 'c')

cc = meson.get_compiler('c')

version_h = custom_target(
  'version.h',
  output: 'version.h',
  command: ['echo', '#define BUSTLE_VERSION "', meson.project_version(), '"'],
  capture: true,
)

subdir('c-sources')
