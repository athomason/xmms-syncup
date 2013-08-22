lib_LTLIBRARIES = libxmms-syncup.la

libdir = @XMMS_GENERAL_PLUGIN_DIR@

AM_CFLAGS = -Wall @XMMS_CFLAGS@ @CFLAGS@ -I$(top_builddir) -I$(top_srcdir) @GTK_CFLAGS@

LIBS = @XMMS_LIBS@
libxmms_syncup_la_LDFLAGS = -module -avoid-version
libxmms_syncup_la_SOURCES =  syncup.c
