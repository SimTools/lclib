
include $(LCLIBROOT)/config/Makefile.lclib

all:
%:
	@if [ $@ == depend ] ; then \
	(cd genww ; make Makefile.d ) ; fi 
	(cd genww ; make $@ )
	@if [ $@ == clean -o $@ == distclean ] ; then \
	( $(RM) $(CLEANFILES) $(DISTCLEANFILES)) ; fi
