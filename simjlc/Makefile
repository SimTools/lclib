
include $(LCLIBROOT)/config/Makefile.lclib

all:
%:
	(cd src ; make $@ ) 
	@if [ $@ == clean -o $@ == distclean ] ; then \
	( $(RM) $(CLEANFILES) $(DISTCLEANFILES)) ; fi
