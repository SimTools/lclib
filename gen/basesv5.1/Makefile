%:
	(cd src; make $@ )
	(cd full ; make $@)
	@if [ $@ == clean -o $@ == distclean ]  ; then \
	 rm -f *~ core *.a  ; fi 

