include $(LCLIBROOT)/config/Makefile.lclib

COMMANDS = allconv chname f2unix

ifeq ($(LCLIB_PLATFORM),aix)
all::
	@echo "*** f2unix is not supported on AIX ***"
install::
else
all:: chname f2unix
install::
	@for i in $(COMMANDS); do \
	    cp -p $$i ../../../bin; \
	done
endif

chname: chname.o
	$(CC) -o chname chname.c

f2unix: f2unix.o
	$(CC) -o f2unix f2unix.c

clean::
	rm -f chname f2unix *.o *~ *.bak

