#ifndef IO_LIB_H
#define IO_LIB_H

#include "matrix_lib.hpp"
#include <stdint.h>

/* read one integer from stdin */
int64_t get_int();

/* write one integer to stdout */
void put_int(int64_t);

/* read one float from stdin */
float get_float(float);

/* write one float to stdout */
void put_float();

/* print a matrix */
void put_mat(struct matrix_int* m);
void put_mat(struct matrix_float* m);
/* print a vector */
void put_vec(struct vector_int *v);
void put_vec(struct vector_float *v);
#endif
