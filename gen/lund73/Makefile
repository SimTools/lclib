all:
	(cd jetset73 ; xmkmf ; make)
	(cd full ; xmkmf ; make)

install:
	(cd full ; make install) 

clean:
	(cd jetset73 ; make clean)
	(cd full ; make clean)
	rm -f *~ core *.a
