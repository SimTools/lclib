# -----------------------------------------------
# Makefile to create HELASCHK library
#
# (Author) Akiya Miyamoto, KEK, 17-Jan-2003
#          Recoded so as not to use Imakefile
#$Id$
# -----------------------------------------------

include $(LCLIBROOT)/config/Makefile.lclib

LIBNAME	= HELASCHKV204

OBJS    = helas_check.o

ALLOBJ  = $(OBJS) 

# -------------------------------------------------

all: lib$(LIBNAME).$(LibSuf)

lib$(LIBNAME).$(LibSuf): $(ALLOBJ)
	$(LibraryTarget)

install: lib$(LIBNAME).$(LibSuf)
	$(InstallLibrary)


# -------------------------------------------------
# Default rules

include $(LCLIBROOT)/config/Makefile.defaults

