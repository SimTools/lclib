# -----------------------------------------------
# Makefile to create HELAS library
#
# (Author) Akiya Miyamoto, KEK, 17-Jan-2003
#          Recoded so as not to use Imakefile
#$Id$
# -----------------------------------------------

include $(LCLIBROOT)/config/Makefile.lclib

LIBNAME = HELASV204

OBJS  =  boostx.o coup1x.o coup2x.o coup3x.o coup4x.o eaixxx.o \
	 eaoxxx.o fsixxx.o fsoxxx.o fvixxx.o fvoxxx.o hioxxx.o \
	 hsssxx.o hssxxx.o hvsxxx.o hvvsxx.o hvvxxx.o iosxxx.o \
	 iovxxx.o ixxxxx.o j3xxxx.o jeexxx.o jioxxx.o jssxxx.o \
	 jvssxx.o jvsxxx.o jvvxxx.o jw3wxx.o jwwwxx.o mom2cx.o \
	 momntx.o oxxxxx.o rotxxx.o ssssxx.o sssxxx.o sxxxxx.o \
	 vssxxx.o vvssxx.o vvsxxx.o vvvxxx.o vxxxxx.o w3w3xx.o \
	 wwwwxx.o

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

