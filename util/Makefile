
include $(LCLIBROOT)/config/Makefile.lclib

SHELL   = /bin/bash
MFLAGS	=
CURRDIR	= .

SUBDIR1	= fortlib genutil lcfull tbs
SUBDIR2	= cli com hpktohbk ltkcl seedbk 
SUBDIRS = $(SUBDIR1) $(SUBDIR2)

all:
%:
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS); do \
	(cd $$i; echo ``making'' $@ ``in $(CURRDIR)/$$i...''; \
	$(MAKE) $(MFLAGS) $@); \
	done
	@if [ $@ == clean -o $@ == distclean ] ; then \
	  $(RM) $(CLEANFILES) $(DISTCLEANFILES) ; fi
