# -----------------------------------------------
# Makefile to create SIMJLC library
#
# (Author) Akiya Miyamoto, KEK, 18-Jan-2003
#          Recoded so as not to use Imakefile
#$Id$
# -----------------------------------------------

include $(LCLIBROOT)/config/Makefile.lclib

LIBNAME	= SIMJLC
BASEDIR = ../..

OBJS   = dscsmr.o  dscswm.o  smrblk.o smrcbk.o  smrcht.o  smrcls.o \
	 smrcvt.o  smremc.o  smrevt.o smrhdc.o  smrhle.o  smrhlx.o \
	 smrjin.o  smrjtm.o  smrprd.o smrrin.o  smrrtm.o  smrsha.o \
	 smrshw.o  smrtbg.o  smrvtx.o swmblk.o  swmjin.o \
	 smrvhl.o  smrcmo.o  smrhlm.o \
	 swmjtm.o  swmprd.o  swmrin.o swmrtm.o  uftvtk.o ulcvtk.o 

INCPATH = -I. -I$(BASEDIR)/util/lcfull/src \
	  -I$(BASEDIR)/util/fortlib/src

ALLOBJ  = $(OBJS) swmevt.o

SRCS    = $(subst .o,.F,$(ALLOBJ))

INCS    = $(wildcard *.inc)

#
# --------------------------------------------------
#

all: lib

lib: lib$(LIBNAME).a

lib$(LIBNAME).$(LibSuf): $(ALLOBJ)
	$(LibraryTarget)

install: lib$(LIBNAME).$(LibSuf)
	$(InstallLibrary)


ifeq ($(LCLIB_PLATFORM),hpux)
swmevt.o: swmevt.F
	f77 $< -c $(FFLAGSE) $(INCPATH) -o swmevt.o
endif

# ----------------------------------------
include $(LCLIBROOT)/config/Makefile.defaults

