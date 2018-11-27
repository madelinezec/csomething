stdlib.a: io_lib.o matrix_lib.o
	ar rcs stdlib.a io_lib.o matrix_lib.o

io_lib.o: io_lib.cpp
	g++ -c io_lib.cpp -o io_lib.o

matrix_lib.o: matrix_lib.cpp
	g++ -c matrix_lib.cpp -o matrix_lib.o

clean:
	rm -f *.o *a
