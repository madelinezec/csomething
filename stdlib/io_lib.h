#ifndef IO_LIB_H
#define IO_LIB_H

#include "matrix_lib.h"
#include <stdint.h>

/* read one integer from stdin */
uint64_t get_int();

/* write one integer to stdout */
void put_int(uint64_t);

/* read one float from stdin */
float get_float(float);

/* write one float to stdout */
void put_float();

/* print a matrix */
void put_mat(struct matrix* m);

/* print a vector */
void put_vec(struct vector *v);
#endif