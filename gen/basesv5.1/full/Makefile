# -----------------------------------------------
# Makefile to create Basesv5.1 library
#
# (Author) Akiya Miyamoto, KEK, 15-Jan-2003
#          Recoded so as not to use Imakefile
#$Id$
# -----------------------------------------------

include $(LCLIBROOT)/config/Makefile.lclib

BASEDIR = ../../..
LIBNAME = BASESV5.1

INCPATH = -I$(BASEDIR)/util/lcfull/src \
	  -I$(BASEDIR)/util/com 
SRCDIR	= ../src

OBJS    = dsbs51.o bs51ri.o bs51rt.o bs51ji.o bs51ev.o \
	bs51rb.o bs51wb.o bs51rs.o bs51dt.o usrout.o

OBJF	= bases.o bhinit.o bhplot.o bhrset.o bhsave.o \
	bhsum.o bscast.o bschck.o bsdate.o bsdsum.o bsetgu.o \
	bsetgv.o bsinfo.o bsinit.o bsdims.o bsgrid.o bsparm.o \
	bsintg.o bsisum.o bslist.o bsordr.o bsprnt.o bsread.o \
	bstcnv.o bstime.o bsunix.o bsutim.o bswrit.o dhfill.o \
	dhinit.o dhplot.o drloop.o drn.o shcler.o shfill.o \
	shinit.o shplot.o shrset.o shupdt.o spchck.o sphist.o \
	spinfo.o sprgen.o spring.o xhchck.o xhfill.o xhinit.o \
	xhordr.o xhplot.o xhrnge.o xhsave.o xhscle.o


SRCF  = $(patsubst %.o,$(SRCDIR)/%.f,$(OBJF))
OOBJF = $(patsubst %.o,$(SRCDIR)/%.o,$(OBJF))

OBJC	= uxdate.o uxdate_.o uxtime.o uxtime_.o

SRCC  = $(patsubst %.o,$(SRCDIR)/%.c,$(OBJC))
OOBJC = $(patsubst %.o,$(SRCDIR)/%.o,$(OBJC))

#######################

ALLOBJ     = $(OBJS) $(OBJF) $(OBJC)

INCS = $(wildcard *.inc)

# -------------------------------------------------

all: lib$(LIBNAME).a

lib$(LIBNAME).$(LibSuf):: $(ALLOBJ)
	$(LibraryTarget)

install:: lib$(LIBNAME).$(LibSuf)
	$(InstallLibrary)

$(OOBJF): $(SRCF)
	(cd ../src; make)

$(OOBJC): $(SRCC)
	(cd ../src; make)

$(OBJF): $(OOBJF)
	ln -sf ../src/$*.o $*.o

$(OBJC): $(OOBJC)
	ln -sf ../src/$*.o $*.o

# ------------------------------------------------------

bs51dt.o: bs51pr.inc
bs51ev.o: bs51pr.inc
bs51ji.o: bs51pr.inc $(BASEDIR)/util/lcfull/src/prgenf.inc
bs51ri.o: bs51pr.inc $(BASEDIR)/util/lcfull/src/prgenf.inc
bs51rt.o: bs51pr.inc $(BASEDIR)/util/lcfull/src/prgenf.inc
dsbs51.o: $(BASEDIR)/util/lcfull/src/fldscr.inc \
	  $(BASEDIR)/util/lcfull/src/fllevl.inc \
	  $(BASEDIR)/util/lcfull/src/prjflg.inc 


# --------------------------------------------------------

include $(LCLIBROOT)/config/Makefile.defaults
