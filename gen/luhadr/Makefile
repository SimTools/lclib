# -----------------------------------------------
# Makefile to create LUHADR library
#
# (Author) Akiya Miyamoto, KEK, 17-Jan-2003
#          Recoded so as not to use Imakefile
#$Id$
# -----------------------------------------------

include $(LCLIBROOT)/config/Makefile.lclib

LIBNAME	= LUHADR
BASEDIR = ../..

OBJS = dslhad.o flnbru.o flneru.o flnpar.o \
	lufrag.o luhadr.o lutuhl.o ulctau.o

SRCS  = $(patsubst %.o,%.F,$(OBJS))

ALLOBJ  = $(OBJS) 

INCPATH = -I$(BASEDIR)/util/lcfull/src \
	  -I$(BASEDIR)/util/com 

# -------------------------------------------------

all: lib$(LIBNAME).$(LibSuf)

lib$(LIBNAME).$(LibSuf): $(ALLOBJ)
	$(LibraryTarget)

install: lib$(LIBNAME).$(LibSuf)
	$(InstallLibrary)

# ---------------------------------------------------
dslhad.o: $(BASEDIR)/util/lcfull/src/fldscr.inc \
	 $(BASEDIR)/util/lcfull/src/fllevl.inc \
	 $(BASEDIR)/util/lcfull/src/prjflg.inc
flneru.o: $(BASEDIR)/util/com/comode.inc

# --------------------------------------------------
include $(LCLIBROOT)/config/Makefile.defaults
