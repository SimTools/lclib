# -----------------------------------------------
# Makefile to create FORTLIB library
#
# (Author) Akiya Miyamoto, KEK, 15-Jan-2003
#          Recoded so as not to use Imakefile
#$Id$
# -----------------------------------------------

include $(LCLIBROOT)/config/Makefile.lclib

OBJS	= bases.o bhinit.o bhplot.o bhrset.o bhsave.o \
	bhsum.o bscast.o bschck.o bsdate.o bsdsum.o bsetgu.o \
	bsetgv.o bsinfo.o bsinit.o bsdims.o bsgrid.o bsparm.o \
	bsintg.o bsisum.o bslist.o bsordr.o bsprnt.o bsread.o \
	bstcnv.o bstime.o bsunix.o bsutim.o bswrit.o dhfill.o \
	dhinit.o dhplot.o drloop.o drn.o shcler.o shfill.o \
	shinit.o shplot.o shrset.o shupdt.o spchck.o sphist.o \
	spinfo.o sprgen.o spring.o xhchck.o xhfill.o xhinit.o \
	xhordr.o xhplot.o xhrnge.o xhsave.o xhscle.o uxtime.o \
	uxtime_.o uxdate_.o uxdate.o

all:: $(OBJS)

install: all

include $(LCLIBROOT)/config/Makefile.defaults
