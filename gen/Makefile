SHELL   = /bin/bash
MFLAGS	=
CURRDIR	= .
SUBDIRS	= basesv5.1 helasv204 luhadr tauola

all:
%:
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS); do \
	(cd $$i; echo ``making'' $@ ``in $(CURRDIR)/$$i...''; \
	$(MAKE) $(MFLAGS) $@); \
	done
	@if [ $@ == clean -o $@ == distclean ] ; then \
	  rm -f *~ core ; fi


