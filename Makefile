all: install

dir:	
	@./bin/make.libdir

install: dir
	(cd gen; make install)
	(cd simjlc; make install)
	(cd util  ; make install)

clean:
	(cd gen; make clean)
	(cd simjlc; make clean)
	(cd util  ; make clean)
	rm -f *~

cleanall: clean
	rm -f jobout.install
	(cd gen/basesv5.1/full ; rm -f Makefile)
	(cd gen/basesv5.1/src  ; rm -f Makefile)
	(cd gen/helasv204 ; rm -f */Makefile)
	(cd gen/luhadr ; rm -f */Makefile)
	(rm -f gen/tauola/Makefile)
	(rm -f util/cli/Makefile)
	(rm -f util/com/Makefile)
	(rm -f util/commands/build/Makefile)
	(rm -f util/fortlib/*/Makefile)
	(rm -f util/genutil/src/Makefile)
	(rm -f util/hpktohbk/Makefile)
	(rm -f util/lcfull/src/Makefile)
	(cd util/tbs/src ; rm -f Makefile)
	(cd util/tbschk ; rm -f Makefile tbschk)
	(cd util/lcfull/src ; rm -f prmain.o )
	(cd bin; rm -f tbschk build)
	(cd lib; rm -f *.a)

