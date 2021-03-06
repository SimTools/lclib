# -----------------------------------------------
# Makefile to create FORTLIB library
#
# (Author) Akiya Miyamoto, KEK, 15-Jan-2003
#          Recoded so as not to use Imakefile
#$Id$
# -----------------------------------------------

include $(LCLIBROOT)/config/Makefile.lclib
ifeq ($(JSFCONFIGFC),gfortran)
  ifeq ($(BUILDBITS),-m64)
    FFLAGS += -fno-range-check
  endif
endif

LIBNAME = FORTLIB


OBJL = 	 area.o curfit.o determ.o fchisq.o gqdriv.o \
	 gqfunc.o iibits.o integ.o interp.o jibits.o linfit.o \
	 matinv.o nucrmv.o polfit.o polft2.o \
	 pueloc.o rueloc.o sueloc.o uabsv3.o uadd3.o uadd4.o \
	 ubasv3.o ubcpyh.o ubinit.o ubstbk.o \
	 ubstfd.o ubtran.o ucdate.o ucdxda.o uchang.o uchiin.o \
	 uchisq.o ucmach.o ucolln.o uconex.o uconf8.o \
	 uconfl.o uconj4.o uconst.o ucontr.o ucopv3.o ucopv4.o \
	 ucopy.o ucpick.o ucpser.o ucrclx.o ucrft2.o ucrosd.o \
	 ucross.o ucstrp.o uctime.o uctlsz.o \
	 uctokn.o uctran.o ucylx.o udaydf.o udayfc.o udcay2.o \
	 udcmpv.o udelta.o udist.o udot2.o udot3.o \
	 udot3d.o udot4.o udsrch.o udumph.o uerini.o uerloc.o \
	 uerror.o uertrm.o ufclsp.o ufdcap.o ufgeob.o ufgeoc.o \
	 ufgeoi.o ufgeok.o ufgeot.o ufill.o ufltrk.o ufpair.o \
	 uftvtx.o ufvrtx.o ugauin.o ugradx.o uhangl.o uhifit.o \
	 uhlxcy.o ui2swp.o uidate.o uinimz.o ulincy.o \
	 umatig.o umatin.o umcsth.o uncros.o \
	 undot3.o unrmv3.o unulv3.o unulv4.o uopang.o up2hlx.o \
	 uphelx.o uphian.o uplng.o uplnx.o uplv.o uprbin.o \
	 upstar.o uptpl.o uptplv.o uptv.o uputbt.o uradls.o \
	 urot2.o usclm3.o usetbt.o usetrf.o usetv4.o usortc.o \
	 usorti.o usortr.o ustrnm.o usub3.o usub4.o \
	 utakbt.o utee.o utime.o utrkav.o utrkiv.o utrkmv.o \
	 uurfit.o uwswap.o uxfyn.o uxsort.o uzero.o \
	 uclowr.o \
	 utrkmo.o utrkao.o utrkio.o

OBJASM = nuaddr.o uvset.o uvget.o uvcopy.o uvsetc.o \
	 uvcpyh.o narg.o ucuper.o uvzero.o \
	 errtra.o ucebc.o clockm.o \
	 errset.o uitime.o termid.o \
	 uvfill.o

OBJDEP = clock.o time.o date.o

ifeq ($(LCLIB_PLATFORM),hpux)
OBJDEP = date.o clock.o
endif

INCS = $(wildcard *.inc)

SSL2DIR = ../ssl2

OBJSSL2 = dcgsm.o dcsgm.o dseig1.o hoqrvd.o hqriid.o

SRCSSL2  = $(patsubst %.o,$(SSL2DIR)/%.f,$(OBJSSL2))
OOBJSSL2 = $(patsubst %.o,$(SSL2DIR)/%.o,$(OBJSSL2))

INCS = $(wildcard *.inc)

ALLOBJ = $(OBJL) rann.o ran_.o $(OBJASM) $(OBJDEP) $(COBJECT) $(OBJSSL2)

INCPATH += -I../../lcfull/src  -I./

# -------------------------------------------------

all: lib$(LIBNAME).a

$(OOBJSSL2): $(SRCSSL2)
	(cd ../ssl2; make all )

$(OBJSSL2): $(OOBJSSL2)
	ln -sf ../ssl2/$*.o $*.o

lib$(LIBNAME).$(LibSuf): $(ALLOBJ)
	$(LibraryTarget)

install: lib$(LIBNAME).$(LibSuf)
	$(InstallLibrary)

# 
# ----------------------------------------------------
#
ifneq ($(LCLIB_PLATFORM),hpux)

time.o: ppc/time.c
	$(CC) $(CFLAGS) -c ppc/time.c -o time.o

date.o: ppc/date.c
	$(CC) $(CFLAGS) -c ppc/date.c -o date.o

uitime.o: ppc/uitime.c
	$(CC) $(CFLAGS) -c ppc/uitime.c -o uitime.o

utime.o: ppc/utime.c
	$(CC) $(CFLAGS) -c ppc/utime.c -o utime.o

endif

#
ucdxda.o: unumcn.inc prfeld.inc
uconst.o: unumcn.inc
ucpser.o: unumcn.inc prfeld.inc
uerini.o: uskalr.inc ujbtyp.inc
uerloc.o: ujbtyp.inc
uerror.o: ujbtyp.inc uskalr.inc
uertrm.o: uskalr.inc
ufclsp.o: unumcn.inc prfeld.inc
ufdcap.o: prfeld.inc unumcn.inc
ufgeob.o: ufgeop.inc
ufgeoc.o: ufgeoc.inc ufgeop.inc
ufgeoi.o: ufgeoc.inc ufgeop.inc prfeld.inc
ufgeok.o: ufgeoc.inc ufgeop.inc
ufgeot.o: ufgeoc.inc ufgeop.inc
ufpair.o: unumcn.inc prfeld.inc
uftvtx.o: unumcn.inc prfeld.inc
ufvrtx.o: unumcn.inc prfeld.inc
uhlxcy.o: unumcn.inc prfeld.inc
uphelx.o: unumcn.inc prfeld.inc
utrkav.o: unumcn.inc prfeld.inc
utrkiv.o: unumcn.inc
utrkmv.o: unumcn.inc prfeld.inc

# --------------------------------------------------

include $(LCLIBROOT)/config/Makefile.defaults

