SUBDIRS = dhelas helas helaschk

all:
%:
	for i in $(SUBDIRS); do \
	(cd $$i ; make $@) ; \
	done
	@if [ $@ == clean -o $@ == distclean ] ; then \
	rm -f *~ core ; fi

