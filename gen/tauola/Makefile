# -----------------------------------------------
# Makefile to create TAUOLA library
#
# (Author) Akiya Miyamoto, KEK, 17-Jan-2003
#          Recoded so as not to use Imakefile
#$Id$
# -----------------------------------------------

include $(LCLIBROOT)/config/Makefile.lclib

LIBNAME	= TAUOLA
BASEDIR = ../..

OBJS  = choice.o dcdmas.o formf.o glibk.o inimas.o iniphy.o \
	initdk.o lunpik.o pachep.o photos.o taudec.o taufil.o \
	tauint.o tauola.o taurdf.o tautrm.o tralo4.o luhepc.o pyhepc.o

SRCS  = $(patsubst %.o,%.F,$(OBJS))

ALLOBJ  = $(OBJS) 

# -------------------------------------------------

all: lib$(LIBNAME).$(LibSuf)

lib$(LIBNAME).$(LibSuf): $(ALLOBJ)
	$(LibraryTarget)

install: lib$(LIBNAME).$(LibSuf)
	$(InstallLibrary)

tar:
	rm -f *.o; rm -f *.a; rm -f *~ 
	(cd ../;   tar cvf   lib.tar lib)
	(cd ../;   compress  lib.tar)
	(cd ../;   chmod -R 700 *)


###

choice.o: parmas.inc
dcdmas.o: parmas.inc
inimas.o: parmas.inc
iniphy.o: qedprm.inc
initdk.o: decpar.inc parmas.inc taubra.inc taukle.inc decomp.inc
lunpik.o: taukle.inc
pachep.o: hepevt.inc
taufil.o: parmas.inc idfc.inc taupos.inc p4tau.inc
tauint.o: jaki.inc idfc.inc inout.inc idpart.inc taurad.inc testa1.inc
taurdf.o: taukle.inc taubra.inc
tralo4.o: parmas.inc p4tau.inc

# --------------------------------------------------
include $(LCLIBROOT)/config/Makefile.defaults

