# -----------------------------------------------
# Makefile to create LTKCL library
#
# (Author) Akiya Miyamoto, KEK, 15-Jan-2003
#          Recoded so as not to use Imakefile
#$Id$
# -----------------------------------------------

include $(LCLIBROOT)/config/Makefile.lclib

LIBNAME	= LTKCL

OBJS = chkcls.o cmbblk.o cmbtrk.o fglcls.o gamtrk.o \
	gtcdtk.o gtclid.o hadtrk.o hstcel.o inicls.o \
	lpttrk.o prtcel.o prtcls.o rmvlpt.o ufclst.o usrtih.o

INCS = $(wildcard *.inc)

INCPATH = -I. -I../../util/lcfull/src -I../../simjlc/src \
	  -I../../util/fortlib/src 

ALLOBJ = $(OBJS)

SRCS = $(subst .o,.F,$(OBJS))

# -------------------------------------------------

all: lib$(LIBNAME).$(LibSuf)

lib$(LIBNAME).$(LibSuf): $(ALLOBJ)
	$(LibraryTarget)

install: lib$(LIBNAME).$(LibSuf)
	$(InstallLibrary)

# ------------------------------------------------------

include $(LCLIBROOT)/config/Makefile.defaults
