stdlib.a: io_lib.o matrix_lib.o
	ar rcs stdlib.a io_lib.o matrix_lib.o

io_lib.o: io_lib.cpp
	c++ -g -c io_lib.cpp -o io_lib.o

matrix_lib.o: matrix_lib.c
	cc -g -c -fopenmp matrix_lib.c -o matrix_lib.o

clean:
	rm -f *.o *a
