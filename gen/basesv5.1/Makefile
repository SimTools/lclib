all:
	(cd src ; xmkmf ; make)
	(cd full ; xmkmf ; make)

install:
	(cd src ; make install)
	(cd full ; make install)

clean:
	(cd src ; make clean)
	(cd full ; make clean)
	rm -f *~ core *.a
	
