all:
	(cd dhelas ; xmkmf ; make)
	(cd helas ; xmkmf ; make)
	(cd helaschk ; xmkmf ; make)
install:
	(cd dhelas ; make install)
	(cd helas ; make install)
	(cd helaschk ; make install)

clean: 
	(cd dhelas ; make clean)
	(cd helas ; make clean)
	(cd helaschk ; make clean)
	rm -f *~ core
