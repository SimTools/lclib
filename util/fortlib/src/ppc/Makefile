PROGRAM = test

all:: $(PROGRAM)

OBJS = date.o time.o uitime.o utime.o

$(PROGRAM):: $(OBJS)
	$(F77) -o $(PROGRAM) $(PROGRAM).F $(OBJS)

clean::
	rm *.o $(PROGRAM) core *~
