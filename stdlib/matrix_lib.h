#ifndef MATRIX_LIB_H
#define MATRIX_LIB_H

#include <stddef.h>
/* these two are opaque types.
 * they should be internal to the implementation
 * our codegen should not concern itself with
 * the layout of these objects */

/* possible memory layout: 
 * struct matrix {
 *     size_t m, n;
 *     int type; // 0 = int, 1 = float, 2 = double, etc.
 *     void *data; // the actual matrix entries on the heap 
 * }
 */
struct matrix;
struct vector;

/* allocates an m*n matrix of type int on the heap */
struct matrix *alloc_mat_int(size_t m, size_t n);

/* allocate matrix of floats */
struct matirx *alloc_mat_float(size_t m, size_t n);

/* allocate an array of integers of size n */
struct vector *alloc_vec_int(size_t n);

/* allocate an array of floats */
struct vector *alloc_vec_float(size_t n);

/* this should call whatever destructor necessary and call free */
void free_matrix(struct vector *m);

void free_vector(struct vector *v);

/* fill the matrix m with entries from one-dimensional array "data" */
void fill_mat_int(struct matrix *m, int *data);

void fill_mat_float(struct matrix *m, float *data);

void fill_vec_int(struct vector *v, int *data);

void fill_vec_float(struct vector *v, float *data);

/* In case of index-out-of-bounds, the functions should
 * write to stderr and exit the process */

/* it should return a pointer to the memory
 * location holding m[i][j],
 * so it could be read from or written to by
 * the generated program */
int *get_index_matrix_int(struct matrix *m, size_t i, size_t j);

/* similar to the previous function */
float *get_index_matrix_float(struct matrix *m, size_t i, size_t j);

int *get_index_vec_int(struct vector *v, size_t i);

float *get_index_vec_float(struct vector *v, size_t i);


/* the following are linear algebra operations */

void addition_mat_int(struct matrix *dest, struct matrix *arg1, struct matrix *arg2);
void addition_mat_float(struct matrix *dest, struct matrix *arg1, struct matrix *arg2);
void addition_vec_int(struct vector *dest, struct vector *arg1, struct vector *arg2);
void addition_vec_float(struct vector *dest, struct vector *arg1, struct vector *arg2);

void scalar_product_mat_int(struct matrix *dest, int arg1, struct matrix *arg2);
void scalar_product_mat_float(struct matrix *dest, int arg1, struct matrix *arg2);
void scalar_product_vec_int(struct vector *dest, int arg1, struct vector *arg2);
void scalar_product_vec_float(struct vector *dest, int arg1, struct vector *arg2);

void mat_product_int(struct matrix *dest, struct matrix *arg1, struct matrix *arg2);
void mat_product_float(struct matrix *dest, struct matrix *arg1, struct matrix *arg2);

void mat_inv_int(struct matrix *dest, struct matrix *src);
void mat_inv_float(struct matrix *dest, struct matrix *src);

// more operations to follow

#endif
