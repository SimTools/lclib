# -----------------------------------------------
# Makefile to create CLI library
#
# (Author) Akiya Miyamoto, KEK, 15-Jan-2003
#          Recoded so as not to use Imakefile
#$Id$
# -----------------------------------------------

include $(LCLIBROOT)/config/Makefile.lclib

LIBNAME = COM

OBJS  = chval.o comctrlc.o comhelp.o comutl.o crange.o inquire.o \
	modbits.o modval.o procmode.o showit.o switch.o telbit.o \
	tellit.o screen.o signal.o

ALLOBJ = $(OBJS)

INCS = $(wildcard *.inc)

LIBCOM = libCOM.$(LibSuf)
LIBCLI = ../../lib/libCLI.$(LibSuf)

DISTCLEANFILES += comtest SYS*

# -------------------------------------------------

all: lib$(LIBNAME).a

lib$(LIBNAME).$(LibSuf): $(ALLOBJ)
	$(LibraryTarget)

install: lib$(LIBNAME).$(LibSuf)
	$(InstallLibrary)

# --------------------------------------------------

comtest:: comtest.o $(LIBCOM) $(LIBCLI)
	$(FC) -o comtest comtest.o $(LIBCOM) $(LIBCLI)

# ---------------------------------------
chval.o:	comode.inc
comhelp.o:	comode.inc
comhelpo.o:	comode.inc
comctrlc.o:	comode.inc
comtest.o:	comode.inc
comtst.o:	comode.inc
comutl.o:	comode.inc
crange.o:	comode.inc
inquire.o:	comode.inc
modbits.o:	comode.inc
modval.o:	comode.inc
showit.o:	comode.inc
switch.o:	comode.inc
telbit.o:	comode.inc
tellit.o:	comode.inc

# -------------------------------------------------

include $(LCLIBROOT)/config/Makefile.defaults
