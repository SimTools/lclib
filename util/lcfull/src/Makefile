# -----------------------------------------------
# Makefile to create LCFULL library
#
# (Author) Akiya Miyamoto, KEK, 15-Jan-2003
#          Recoded so as not to use Imakefile
#$Id$
# -----------------------------------------------

include $(LCLIBROOT)/config/Makefile.lclib

INCPATH = -I../../lcfull/src \
	  -I../../fortlib/src  \
	  -I../../tbs/src

OBJS  = dbcanl.o dbcini.o dbcopr.o dbcred.o dbcsex.o dbcsub.o dbxadc.o \
	dbxado.o dbxfdc.o dbxfdo.o dscusr.o prblk1.o prcdyn.o \
	prcevt.o prched.o prcred.o prdbin.o prdbnk.o prdis.o previn.o \
	prevtm.o prgcns.o prgevt.o prgfid.o prgvrs.o prinit.o \
	prjobi.o prjobt.o prjsum.o prjtit.o probnk.o \
	prpdsc.o prpevo.o prpevt.o prpvrs.o prrbnk.o prrinp.o prrnin.o \
	prrpar.o prrpin.o prrsum.o prruni.o prrunt.o prsdtp.o prsfid.o \
	prslvl.o prtbs1.o prtbs2.o prucom.o \
	pruhed.o prujin.o prujtm.o pruptr.o prurin.o prurtm.o \
	pruser.o prwcns.o prwdtp.o prwern.o prwerr.o prwjpr.o \
	pueloc.o prcseq.o prgtif.o 

# prlrnl.o prtrgt.o prtrg1.o prtrg2.o  prinfo.o

OBJC =  closelcfull.o
ifeq ($(FC),gfortran)
OBJC += dbcwrapper.o
endif

OBJF =  extsub.o fbkdel.o fblchk.o fblcre.o fdfanl.o fdoevt.o \
	fendrn.o fevusr.o fexevt.o fexprn.o fexsub.o finirn.o flclss.o \
	flinis.o fulenv.o 

#OBJM   = mablkd.o maclos.o macvrs.o madef.o madump.o magete.o \
#	 mahelp.o mainit.o makred.o maldir.o maldsn.o maldst.o \
#	 malrun.o mamsg.o maopen.o maoptn.o mapurg.o maread.o \
#	 maselr.o masrun.o matkni.o mawacc.o maxerr.o maxest.o \
#	 maxget.o maxloc.o maxopn.o maxput.o maxrst.o maxslc.o \
#	 maxstp.o maxvrf.o 

ALLOBJ = $(OBJS) $(OBJF) $(OBJC)
SHROBJ = $(OBJS) $(OBJF) $(OBJC)
MAINOBJ = prmain.o 

LIBNAME	   = LCFULL

SRCS = $(subst .o,.F,$(OBJF) $(OBJS))

INCS = $(wildcard *.inc)

# -------------------------------------------------

all: lib$(LIBNAME).a $(MAINOBJ)

lib$(LIBNAME).$(LibSuf): $(ALLOBJ)
	$(LibraryTarget)

install: lib$(LIBNAME).$(LibSuf)
	$(InstallLibrary)

distclean::
	rm -f prmain.o

# ------------------------------------------------------

include $(LCLIBROOT)/config/Makefile.defaults
