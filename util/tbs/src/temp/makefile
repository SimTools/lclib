all:: test

CFLAGS = -g
OBJS = test2.o lun_to_fileno_ppc.o tb_buflib.o
test:: test1.F $(OBJS)
	g77 -o test -g test1.F $(OBJS)
clean::
	rm *.o test
