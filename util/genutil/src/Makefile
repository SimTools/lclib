# -----------------------------------------------
# Makefile to create GENUTIL library
#
# (Author) Akiya Miyamoto, KEK, 15-Jan-2003
#          Recoded so as not to use Imakefile
#$Id$
# -----------------------------------------------

include $(LCLIBROOT)/config/Makefile.lclib

LIBNAME	= GENUTIL

OBJL  =  beamfn.o beamgn.o beamin.o boostd.o dbgbos.o decay2.o \
	 dsgels.o dsgpyt.o dssprg.o eprobx.o ewinit.o \
	 genels.o ihdlun.o ihdtpz.o \
	 pythiaif.o  rcovev.o spbegn.o spblst.o spcall.o spfunc.o \
	 spgpar.o spjbtm.o sprnin.o sut2pd.o thrust.o u2vang.o \
	 ud4mas.o udph2b.o udph3b.o udphmm.o udsetf.o udsetr.o \
	 udtran.o uf2uxj.o uh4mas.o uhdmod.o uhph2b.o \
	 uhph3b.o uhph4b.o uhphmm.o uhsetf.o uhsetr.o ulghed.o \
	 ulgprt.o up4mas.o widhig.o widtop.o widw.o widwd.o \
	 widz.o widzd.o \
	 dssquz.o squzev.o jclust.o

ALLOBJ = $(OBJL) 

ifeq ($(shell uname),Darwin)
ALLOBJ += springwrapper.o
endif

INCS = $(wildcard *.inc)

EPROBINC = $(wildcard beaminc/eprob.blk.*)

INCPATH = -I../../lcfull/src  -I../../com

# -------------------------------------------------

all: lib$(LIBNAME).a

lib$(LIBNAME).$(LibSuf): $(ALLOBJ)
	$(LibraryTarget)

install: lib$(LIBNAME).$(LibSuf)
	$(InstallLibrary)

#

beamfn.o:	beamcm.inc
beamgn.o:	beamcm.inc
beamin.o:	beamcm.inc
dbgbos.o:	fldscr.inc fllevl.inc prjflg.inc
dssprg.o:	fldscr.inc fllevl.inc prjflg.inc
dsgels.o:	fldscr.inc fllevl.inc prjflg.inc
dsspyt.o:	fldscr.inc fllevl.inc prjflg.inc
dssquz.o:	fldscr.inc fllevl.inc prjflg.inc
eprobx.o:	$(EPROBINC)
pythiaif.o:	../../com/comode.inc
rcovev.o:	fldscr.inc fllevl.inc prjflg.inc
uf2uxj.o:	fldscr.inc fllevl.inc prjflg.inc

# -------------------------------------------------
include $(LCLIBROOT)/config/Makefile.defaults
