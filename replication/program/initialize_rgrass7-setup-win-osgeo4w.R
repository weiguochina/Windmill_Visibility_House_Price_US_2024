# Source this file at the top of an R or Rmd file using rgrass7 together
# with GRASS installed via OSGeo4W (https://trac.osgeo.org/osgeo4w/) on a
# Windows machine.

# e.g. source(file.path(getwd(), 'rgrass7-setup-win-osgeo4w.R'))

# You can alternatively add the contents to a project-specific .Rprofile.

# Its better to set these variables in an R session rather than in System,
# especially if you have other GIS software like PostGIS installed, which has
# its own GDAL_DATA path. This is also the only solution for Windows users on
# accounts lacking admin rights.

# Note that if you call `library(sf)` after setting these variables, GDAL_DATA
# will be overwritten!

# These paths are current to 2021-10-22 and may change in future. If you get
# stuck, open a question on gis.stackexchange or r-sig-geo. I'm not resourced
# to respond to one-on-one contacts on this topic.

# from /bin/o4w_env.bat:
Sys.setenv('OSGEO4W_ROOT' = file.path('C:', 'OSGeo4W64'))
# NB Change the above if you installed OSGeo4W in a non-standard location.

# from %OSGEO4W_ROOT%/etc/ini/gdal.bat
Sys.setenv('GDAL_DATA' =
             file.path(Sys.getenv('OSGEO4W_ROOT'), 'share', 'gdal'))
Sys.setenv('GDAL_DRIVER_PATH' =
             file.path(Sys.getenv('OSGEO4W_ROOT'), 'bin', 'plugins'))

# from %OSGEO4W_ROOT%/etc/ini/libjpeg.bat (optional?)
Sys.setenv('JPEGMEM' = 1000000)

# from %OSGEO4W_ROOT%/etc/ini/openssl.bat (optional?)
Sys.setenv('OPENSSL_ENGINES' =
             file.path(Sys.getenv('OSGEO4W_ROOT'), 'lib', 'engines-1_1'))

# from %OSGEO4W_ROOT%/etc/ini/proj.bat
Sys.setenv('PROJ_LIB' =
             file.path(Sys.getenv('OSGEO4W_ROOT'), 'share', 'proj'))

# from %OSGEO4W_ROOT%/etc/ini/python3.bat
Sys.setenv('PYTHONHOME' =
             file.path(Sys.getenv('OSGEO4W_ROOT'), 'apps', 'Python39'))
Sys.setenv('PYTHONUTF8' = 1)

# from %OSGEO4W_ROOT%/etc/ini/qt5.bat (optional?)
Sys.setenv('QT_PLUGIN_PATH' = 
             file.path(Sys.getenv('OSGEO4W_ROOT'), 'apps', 'Qt5', 'plugins'))
Sys.setenv('O4W_QT_PREFIX' = 
             file.path(Sys.getenv('OSGEO4W_ROOT'), 'apps', 'Qt5'))
Sys.setenv('O4W_QT_BINARIES' = 
             file.path(Sys.getenv('OSGEO4W_ROOT'), 'apps', 'Qt5', 'bin'))
Sys.setenv('O4W_QT_PLUGINS' = 
             file.path(Sys.getenv('OSGEO4W_ROOT'), 'apps', 'Qt5', 'plugins'))
Sys.setenv('O4W_QT_LIBRARIES' = 
             file.path(Sys.getenv('OSGEO4W_ROOT'), 'apps', 'Qt5', 'lib'))
Sys.setenv('O4W_QT_TRANSLATIONS' = 
             file.path(Sys.getenv('OSGEO4W_ROOT'), 'apps', 'Qt5', 'translations'))
Sys.setenv('O4W_QT_HEADERS' = 
             file.path(Sys.getenv('OSGEO4W_ROOT'), 'apps', 'Qt5', 'include'))
Sys.setenv('O4W_QT_DOC' = 
             file.path(Sys.getenv('OSGEO4W_ROOT'), 'apps', 'Qt5', 'doc'))

# from%OSGEO4W_ROOT%/apps/grass/grass78/etc/env.bat
Sys.setenv('GISBASE' =
             file.path(Sys.getenv('OSGEO4W_ROOT'), 'apps', 'grass', 'grass78'))
Sys.setenv('GRASS_PYTHON' =
             file.path(Sys.getenv('OSGEO4W_ROOT'), 'bin', 'python3.exe'))
Sys.setenv('PYTHONPATH' =
             file.path(Sys.getenv('GISBASE'), 'etc', 'python'))
Sys.setenv('GRASS_PROJSHARE' =
             file.path(Sys.getenv('OSGEO4W_ROOT'), 'share', 'proj'))
Sys.setenv('FONTCONFIG_FILE'=
             file.path(Sys.getenv("GISBASE"), 'etc', 'fonts.conf'))

# NB these next two will vary depending on how your windows install is handled.
# The GRASS7 folder will also not exist on your system on a fresh install until
# you've opened the program GUI at least once. Comment them out if you don't
# want/need addons and/or don't need the rc information (that file just
# remembers the last grass session opened)

Sys.setenv('GISRC' =
             file.path(Sys.getenv('USERPROFILE'), 'AppData', 'Roaming',
                       'GRASS7', 'rc'))
Sys.setenv('GRASS_ADDON_BASE' =
             file.path(Sys.getenv('USERPROFILE'), 'AppData', 'Roaming',
                       'GRASS7', 'addons'))
# lastly,
Sys.setenv('PATH' =
             paste(file.path(Sys.getenv('GISBASE'), 'lib'),
                   file.path(Sys.getenv('GISBASE'), 'bin'),
                   Sys.getenv('GRASS_ADDONS_BASE'),
                   Sys.getenv('O4W_QT_BINARIES'),			 
                   file.path(Sys.getenv('PYTHONHOME'), 'Scripts'), # python3.bat
                   file.path(Sys.getenv('OSGEO4W_ROOT'), 'bin'),   #o4w_env.bat                   ,
                   Sys.getenv("PATH"),
                   sep = .Platform$path.sep))

# Be aware that if you have other Python or GRASS installs on your path already,
# you might need to move `Sys.getenv("PATH")` to the end of the `paste()`.