
include $(LCLIBROOT)/config/Makefile.lclib

.PHONY: include depend 

SUBDIRS = util gen simjlc
# SUBDIRS = gen

TARGETS = include install depend clean distclean

all: install

include:: 
	mkdir -p $(includedir)

install:: include depend 
	@echo "Installing lib for Pythia Version $(PYTHIA_VERSION)"
	@echo "libdir =$(libdir)"
	mkdir -p $(libdir)

liball: depend include
	for v in 5 6 ; do \
	  ( for i in clean install ; do \
	    ( make libdir=$(LCLIBROOT)/lib$$v PYTHIA_VERSION=$$v $$i ); \
	    done ; ) ; \
	done
	ln -s lib6 lib

clean::
	(cd config ; $(MAKE) clean)
	(cd example ; $(MAKE) clean )
	rm -f *~

cleanall:: distclean

distclean:: clean
	(cd bin; rm -f tbschk build)
	(rm -rf $(libdir))
	(rm -rf $(includedir))
	(rm -rf lib5 lib6)

$(TARGETS)::
	for i in $(SUBDIRS); do \
	( cd $$i ; $(MAKE) $@ ); \
	done

