stdlib.a: io_lib.o matrix_lib.o
	ar rcs stdlib.a io_lib.o matrix_lib.o

io_lib.o: io_lib.c
	cc -c io_lib.c -o io_lib.o

matrix_lib.o: matrix_lib.c
	cc -c matirx_lib.c -o matrix_lib.o

clean:
	rm -f *.o *a
