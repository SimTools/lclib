# -----------------------------------------------
# Makefile to create CLI library
#
# (Author) Akiya Miyamoto, KEK, 15-Jan-2003
#          Recoded so as not to use Imakefile
#$Id$
# -----------------------------------------------

include $(LCLIBROOT)/config/Makefile.lclib

LIBNAME = CLI

OBJS =  affirm.o ambrd.o clclos.o clalia.o \
	clatof.o clatoi.o clbeep.o clierr.o clmacr.o clpart.o clsmem.o \
	clsubp.o clvtos.o clword.o echrd.o fdprd.o fltrd.o \
	hexrd.o intrd.o ispace.o keyrd.o \
	lenrd.o  lkbrd.o logrd.o numrd.o printf.o titrd.o \
	tvmenu.o tvoins.o tvslct.o tvwrtl.o txtrd.o \
	miscunix.o cfargs.o clopen.o chard.o iopen.o ldsdsn.o qopen.o \
	tvclr.o tvoptn.o tvpack.o

INCS = $(wildcard *.inc)

COBJS = strupc.o

ALLOBJ = $(OBJS) $(OBJSF) $(COBJS)

# -------------------------------------------------

all: lib$(LIBNAME).a

lib$(LIBNAME).$(LibSuf): $(ALLOBJ)
	$(LibraryTarget)

install: lib$(LIBNAME).$(LibSuf)
	$(InstallLibrary)

# -------------------------------------------------

affirm.o:	_tvdisp.inc
clierr.o:	_unitrd.inc
clsmem.o:	_key_def.inc
keyrd.o:	_unitrd.inc
logrd.o:	_unitrd.inc
miscunix.o:	jpidef.inc
miscvms.o:	jpidef.inc
tvmenu.o:	_key_def.inc
txtrd.o:	_flagrd.inc
cfargs.o:	_cliarg.inc _cstack.inc
chard.o:	_unitrd.inc _flagrd.inc _tvdisp.inc _clitxt.inc \
		_key_def.inc _cliio.inc
qopen.o:       _key_def.inc
tvclr.o:       _tvdisp.inc _tvcmd.inc
tvpack.o:	_hpoptn.inc _tvcomm.inc

# -------------------------------------------------

include $(LCLIBROOT)/config/Makefile.defaults
