#include "matrix_lib.h"
#include <stdint.h>

/* read one integer from stdin */
uint64_t get_int(){
	int * ptr;
	read(0, ptr, sizeof(int));
}

/* write one integer to stdout */
void put_int(uint64_t){
	int * ptr = &uint64_t;
	write(1, ptr, sizeof(ptr));
}

/* read one float from stdin */
float get_float(){
	float * ptr;
	read(0, ptr, sizeof(float));
}

/* write one float to stdout */
void put_float(float){
	float * ptr = &float;
	write(1, ptr, sizeof(ptr));
}

/* print a matrix */
void put_mat(struct matrix* m){
	int rows = m->m;
	int columns = m->n;
	int offset;
	char new_line [] = "\n"
	char white_space [] = " "
	for(int i = 0; i < rows; i++){
		for(int j = 0; j < columns; j++){
			offset = i * num_columns + j;
			write(1, &m->data[offset], sizeof(m->type));
			write(1, white_space, strlen(white_space));
		}
		write(1, new_line, strlen(new_line));
	}
}

/* print a vector */
void put_vec(struct vector *v){
	int length = v->n;
	for(int j = 0; j < length; j++){
		write(1, &m->data[j], sizeof(v->type));
		write(1, white_space, strlen(white_space));
	}
}

