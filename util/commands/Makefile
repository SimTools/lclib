
include $(LCLIBROOT)/config/Makefile.lclib

SUBDIRS = build

all:
%:
	for i in $(SUBDIRS) ; do \
	(cd $$i ; make $@ ); \
	done
	@if [ $@ == clean -o $@ == distclean ] ; then \
	( $(RM) $(CLEANFILES) $(DISTCLEANFILES)) ; fi 

