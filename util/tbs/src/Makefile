# -----------------------------------------------
# Makefile to create TBS library
#
# (Author) Akiya Miyamoto, KEK, 15-Jan-2003
#          Recoded so as not to use Imakefile
#$Id$
# -----------------------------------------------

include $(LCLIBROOT)/config/Makefile.lclib

LIBNAME	= TBS

F_OBJS	= tbaloc.o tbapdh.o tbapnd.o \
	tbdelb.o tbdele.o tbextd.o tbcrte.o \
	tbflnk.o tbget.o tbgeth.o tbinit.o \
	tblbnm.o tblel0.o tblel1.o tblink.o tblist.o tblldt.o \
	tbllnk.o tblnka.o tbloc.o tblspc.o tblsub.o tblsum.o \
	tbluni.o tbmod.o tbmodh.o tbmpt.o tbnoel.o tbpass.o \
	tbpick.o tbpikh.o tbput.o tbputh.o tbqbnk.o tbqewh.o \
	tbqpt.o tbqrec.o tbread.o tbrnam.o tbrrec.o tbshar.o \
	tbsize.o tbsqez.o tbstat.o tbuxin.o tbwarn.o tbwrit.o \
	tbxasl.o tbxbdl.o tbxbex.o tbxcel.o tbxcha.o tbxcks.o \
	tbxcrd.o tbxcrn.o tbxcti.o tbxdel.o tbxdex.o tbxeex.o \
	tbxerr.o tbxfbk.o tbxfel.o tbxfrd.o tbxfsb.o tbxgab.o \
	tbxini.o tbxint.o tbxitc.o tbxlen.o tbxr4u.o tbxr8u.o \
	tbxred.o tbxsbk.o tbxsel.o tbxskp.o tbxspc.o tbxsrt.o \
	tbxunx.o tbxvcv.o tbxvdt.o tbxvfb.o tbxvpt.o \
	tbsetf.o tbshwf.o

SUN_OBJS      = lun_to_fileno.o fd_to_fileno.o

HP_OBJS	      = lun_to_fileno_hp.o lnblnk.o

PPC_OBJS      = lun_to_fileno_ppc.o lnblnk.o

LNX_OBJS      = lun_to_fileno_lnx.o lnblnk.o

AIX_OBJS      = lnblnk.o

C_OBJS	      = tb_buflib.o tbvopn.o tbvred.o

INCS = $(wildcard *.inc)

INCPATH = -I../../lcfull/src  -I../../com

SRCS = $(subst .o,.F,$(F_OBJS))

ifeq ($(LCLIB_PLATFORM),hpux)
  SHROBJ   = $(F_OBJS) $(C_OBJS) $(HP_OBJS)
  ALLOBJ   = $(SHROBJ)
endif

ifeq ($(LCLIB_PLATFORM),hiux)
  SHROBJ   = $(F_OBJS) $(C_OBJS) $(HP_OBJS) FNUM.o.hiuxf77
  ALLOBJ   = $(SHROBJ) $(FNUMHIUX)
endif

ifeq ($(LCLIB_PLATFORM),darwin)
  SHROBJ  = $(F_OBJS) $(C_OBJS) $(PPC_OBJS)
  ALLOBJ  = $(SHROBJ)
endif

ifeq ($(LCLIB_PLATFORM),linux)
  SHROBJ  = $(F_OBJS) $(C_OBJS) $(LNX_OBJS)
  ALLOBJ  = $(SHROBJ)
endif

ifeq ($(LCLIB_PLATFORM),aix)
  SHROBJ  = $(F_OBJS) $(C_OBJS) $(AIX_OBJS)
  ALLOBJ  = $(SHROBJ)
endif

# -------------------------------------------------

all: lib$(LIBNAME).$(LibSuf)

lib$(LIBNAME).$(LibSuf): $(ALLOBJ)
	$(LibraryTarget)

install: lib$(LIBNAME).$(LibSuf)
	$(InstallLibrary)

############################################


ifeq ($(LCLIB_PLATFORM),darwin)
lun_to_fileno_ppc.o: lun_to_fileno_ppc.c
endif

ifeq ($(LCLIB_PLATFORM),linux)
lun_to_fileno_lnx.o: lun_to_fileno_lnx.F
	$(FC) $(FFLAGS) -fno-underscoring -c $*.F
endif

# -----------------------------------------------------
include $(LCLIBROOT)/config/Makefile.defaults
