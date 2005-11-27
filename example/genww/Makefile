#
# Make file for e+e- -> W+W- event generator, simulator, and analysis.
# This makefile is to create three program,
#      nod    ;  bases for e+e- -> W+W- without W decay.
#      bases  ;  bases for e+e- -> W+W- includeing W decay, for simulator
#      anal   ;  make FULL program for spring and analysis.
#
# A. Miyamoto  29-Sep-1994  First official version on HP-UX
#              21-Nov-1994  Modified for use with BASESV5.1
#              16-Feb-1996  For demo at Nagoya Univ. Nakanishi's Lab.
#               8-Dec-1997  To Run on HI-UX
#              24-Dec-1998  To use with cernlib/98a

include $(LCLIBROOT)/config/Makefile.lclib

LCLIB   = $(LCLIBROOT)/lib
CERNLIB = $(CERN_ROOT)/lib

INCPATH += -I$(LCLIBROOT)/include

MAIN  = $(LCLIBROOT)/util/lcfull/src/prmain.o

OBJS   = wwspr.o flanli.o wwdcy.o wwamp.o wwanl.o \
	    coupla.o boost0.o bs51cm.o bshget.o 

CERNLIBOPT = `cernlib packlib jetset74 mathlib`

LIBS   =  -Wl,-L$(LCLIB) -lLCFULL -lLUHADR -lGENUTIL -lTAUOLA \
	    -lHELASV204 -lBASESV5.1 -lSIMJLC -lFORTLIB  -lCOM -lCLI \
	    -lTBS -lLCFULL -lGENUTIL  \
	    -lHPKTOHBK -lFORTLIB -lCOM -lCLI \
	    -Wl,-L$(CERNLIB) $(CERNLIBOPT)

ifeq ($(LCLIBROOT),hpux)
MYLDOPT = -lvec -lU77 -Wl,-a,archive -Wl,-L/lib -lm
endif

CLEANFILES += *.bak BASES.WW.TBS wwanal.ex dis.wwanl ft.01
#
# ##########################################################
#


all : anal

ifeq ($(PYTHIA_VERSION),6)
anal   : $(OBJS)  $(MAIN)
	 @echo "**************"
	 @echo Example for PYTHIA Version 6 is not supported yet
	 @echo See comments in HowToInstall at the top directory
else
anal   : $(OBJS)  $(MAIN)
	 $(FC) $(MAIN) $(OBJS) $(FFLAGS) $(LIBS) -o wwanal.ex 
endif

$(MAIN): ../../util/lcfull/src/prmain.F
	(cd ../../util/lcfull/src ; make prmain.o )

help  :
	@echo "This is a Makefile to create e+e- ->W+W- generator."
	@echo "Following commands are recognized. Read Readme file for more details."
	@echo "    make anal      ; create spring/simulator/analysis job"
	@echo "    make celan     ; cleanup directory "
	@echo "    make           ; get this help "
	@echo "                                       A.Miyamoto  29-Sep-1994"

include $(LCLIBROOT)/config/Makefile.defaults


