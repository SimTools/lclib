all:
	(cd ssl2 ; xmkmf ; make)
	(cd src ; xmkmf ; make)

install: all
	(cd src ; make install)

clean:
	(cd src ; make clean)
	(cd ssl2 ; make clean)
	rm -f *~


