stdlib.a: io_lib.o matrix_lib.o
	ar rcs stdlib.a io_lib.o matrix_lib.o

io_lib.o: io_lib.cpp
	g++ -g -c io_lib.cpp -o io_lib.o

matrix_lib.o: matrix_lib.c
	g++ -g -c matrix_lib.c -o matrix_lib.o

clean:
	rm -f *.o *a
