# -----------------------------------------------
# Makefile to create CLI library
#
# (Author) Akiya Miyamoto, KEK, 25-Jan-2003
#          Recoded so as not to use Imakefile
#$Id$
# -----------------------------------------------

include $(LCLIBROOT)/config/Makefile.lclib

COMMAND = build

OBJS  = build.o buildm.o cpstrp.o ctoken.o cupper.o \
        lbsdsn.o nindex.o ucstrp.o prompt.o cnvnam.o \
        lnblnk.o rindex.o getcwd_.o

CLEANFILES += $(COMMAND)

# -------------------------------------------------

all:: $(COMMAND)
install::
	cp -p $(COMMAND) ../../../bin

$(COMMAND):: $(OBJS)
	$(FC) -g $(FFLAGS) $(OBJS) -o $(COMMAND)

ifneq ($(LCLIB_PLATFORM),linux)
getcwd_.o: getcwd_.F
	$(FC) -g -fno-underscoring -c $*.F
endif

# -------------------------------------------------

-include Makefile.$(LCLIB_PLATFORM)

include $(LCLIBROOT)/config/Makefile.defaults
