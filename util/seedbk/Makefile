# -----------------------------------------------
# Makefile to create SEEDBK library
#
# (Author) Akiya Miyamoto, KEK, 14-Jan-2003
#          Recoded so as not to use Imakefile
#$Id$ 
# -----------------------------------------------

include $(LCLIBROOT)/config/Makefile.lclib

OBJS = dsmcsd.o mcsdpr.o mcsdri.o mcsdrt.o mcseed.o

LIBNAME = SEEDBK

# -------------------------------------------------
all: lib$(LIBNAME).$(LibSuf)

lib$(LIBNAME).$(LibSuf): $(OBJS)
	$(LibraryTarget)

install: lib$(LIBNAME).$(LibSuf)
	$(InstallLibrary)

dsmcsd.o: FLDSCR.inc FLLEVL.inc PRJFLG.inc
mcsdpr.o: MCSDFG.inc
mcsdri.o: MCSDFG.inc
mcsdrt.o: MCSDFG.inc
mcseed.o: MCSDFG.inc

# ------------------------------------------------------

include $(LCLIBROOT)/config/Makefile.defaults
