SHELL   = /bin/bash
MFLAGS	=
CURRDIR	= .

SUBDIR1	= commands fortlib genutil lcfull tbs
SUBDIR2	= cli com hpktohbk ltkcl seedbk tbschk
SUBDIRS = $(SUBDIR1) $(SUBDIR2)

all:
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIR1); do \
	(cd $$i; echo ``making'' all ``in $(CURRDIR)/$$i...''; \
	$(MAKE) $(MFLAGS) all); \
	done
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIR2); do \
	(cd $$i; echo ``making'' all ``in $(CURRDIR)/$$i...''; \
	xmkmf; \
	$(MAKE) $(MFLAGS) all); \
	done
	
install: all
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS); do \
	(cd $$i; echo ``making'' install ``in $(CURRDIR)/$$i...''; \
	$(MAKE) $(MFLAGS) install); \
	done
clean:
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS); do \
	(cd $$i; echo ``making'' clean ``in $(CURRDIR)/$$i...''; \
	$(MAKE) $(MFLAGS) clean); \
	done



