
include $(LCLIBROOT)/config/Makefile.lclib

MFLAGS  =
SUBDIRS = src ssl2

all:
%:
	for i in $(SUBDIRS); do \
	( cd $$i ; $(MAKE) $(MFLAGS) $@); \
	done
	@if [ $@ == clean -o $@ == distclean ] ; then \
	  $(RM) $(CLEANFILES) $(DISTCLEANFILES) ; fi
