#
# Makefile for bases50 test program
#                __
#   e+ e- --> nu nu gamma
#
#   BASESLDIR     bases50 library directory
#   BASESLIB      bases50 library
#
SHELL         = /bin/csh
FC            = fort77
GRACELDIR     = /users/japan/kawabata/basesv5.1/src
BASESLIB      = bases
F77FLAGS      = +ppu

#
INTGEN        = intgen
IGOBJ         = test.o testfunc.o
#
INT           = integ
IOBJ          = testint.o testfunc.o
#
GEN           = gen
GOBJ          = testgen.o testfunc.o
#
LOOP          = loop
LOBJ          = testloop.o testfunc.o

.f.o:
	f77 +ppu -c $*.f -o $*.o

all:            $(INTGEN) 

#$(INT) $(GEN) $(LOOP)

$(INTGEN):   $(IGOBJ) 
	 f77 +ppu $(IGOBJ) -Wl,-L../../lib -lBASESV51 -o test

#		$(FC) $(IGOBJ) -o $(INTGEN) -L$(GRACELDIR) -l$(BASESLIB)

$(INT):   $(IOBJ) $(GRACELDIR)/lib$(BASESLIB).a
		$(FC) $(IOBJ) -o $(INT) -L$(GRACELDIR) -l$(BASESLIB)

$(GEN):   $(GOBJ) $(GRACELDIR)/lib$(BASESLIB).a
		$(FC) $(GOBJ) -o $(GEN) -L$(GRACELDIR) -l$(BASESLIB)

$(LOOP):   $(LOBJ) $(GRACELDIR)/lib$(BASESLIB).a
		$(FC) $(LOBJ) -o $(LOOP) -L$(GRACELDIR) -l$(BASESLIB)

clean:
		\rm -f *.o $(INTGEN) $(INT) $(GEN) $(LOOP)
