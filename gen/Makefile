SHELL	= /bin/sh
MFLAGS	=
CURRDIR	= .
SUBDIRS	= basesv5.1 helasv204 luhadr tauola

all:
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS); do \
	(cd $$i; echo ``making'' all ``in $(CURRDIR)/$$i...''; \
	if [ $$i = luhadr -o $$i = tauola ]; then xmkmf; fi; \
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



