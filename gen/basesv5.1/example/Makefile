# -----------------------------------------------
# Makefile to create example of bases
#
# (Author) Akiya Miyamoto, KEK, 18-Jan-2003
#
#$Id$
# -----------------------------------------------

include $(LCLIBROOT)/config/Makefile.lclib

PROGRAM   = nng.ex
OBJ       = PRMAIN.o Example.o testfunc.o 

CERNLIB = $(CERN_ROOT)/lib

LIB 	= -Wl,-L$(LCLIBROOT)/lib -lBASESV5.1 \
	-lLUHADR -lTAUOLA -lSIMJLC -lLTKCL \
	-lGENUTIL -lHPKTOHBK -lLCFULL -lCOM -lCLI -lTBS -lFORTLIB \
	-Wl,-L$(CERNLIB) -ljetset74 -lpacklib -lmathlib

ifeq ($(LCLIB_PLATFORM),ppc)
LIB	+= -lU77 -lvec -lm
endif

all : $(PROGRAM)

$(PROGRAM) : $(OBJ) 
	 $(FC) $(FFLAGS) -o $(PROGRAM) $(OBJ) $(LIB)

clean:
	rm -f *.o *~ core 
