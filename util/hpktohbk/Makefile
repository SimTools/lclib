#
# Makefile
#  
#
#$Id$

include $(LCLIBROOT)/config/Makefile.lclib

OBJS       = hpktohbk.o

LIBNAME	   = HPKTOHBK

# ######################

all:: lib$(LIBNAME).$(LibSuf)

lib$(LIBNAME).$(LibSuf): $(OBJS)
	$(LibraryTarget)

install: lib$(LIBNAME).$(LibSuf)
	$(InstallLibrary)

# ---------------------------------------------------
include $(LCLIBROOT)/config/Makefile.defaults
