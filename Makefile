all: install

install:
	(cd gen    ; make install)
	(cd simjlc ; make install)
	(cd util   ; make install)

clean:
	(cd gen    ; make clean)
	(cd simjlc ; make clean)
	(cd util   ; make clean)
	rm -f *~


