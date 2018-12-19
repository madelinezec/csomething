#ifndef MATRIX_LIB_H
#define MATRIX_LIB_H

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "matrix_impl.hpp"

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

typedef struct matrix_int Mat_i;
typedef struct matrix_float Mat_f;
typedef struct vector_int Vec_i;
typedef struct vector_float Vec_f;


struct matrix_int {
    size_t m, n;
    int** data; // the actual matrix entries on the heap 
 };
 
struct matrix_float {
    size_t m, n;
    float** data; // the actual matrix entries on the heap 
 };
 
struct vector_int {
    size_t n;
    int* data; // the actual matrix entries on the heap 
 };
 
struct vector_float {
    size_t n;
    float* data; // the actual matrix entries on the heap 
};

/* allocates an m*n matrix of type int on the heap */
Mat_i* alloc_mat_int(size_t m, size_t n);

/* allocate matrix of floats */
Mat_f* alloc_mat_float(size_t m, size_t n);

/* allocate an array of integers of size n */
Vec_i* alloc_vec_int(size_t n);

/* allocate an array of floats */
Vec_f* alloc_vec_float(size_t n);

/* this should call whatever destructor necessary and call free */
void free_matrix(Mat_i* mat);
void free_matrix(Mat_f* mat);

void free_vector(Vec_i* v);
void free_vector(Vec_f* v);

/* fill the matrix m with entries from one-dimensional array "data" 
void fill_mat_int(struct matrix *m, int *data);

void fill_mat_float(struct matrix *m, float *data);

void fill_vec_int(struct vector *v, int *data);

void fill_vec_float(struct vector *v, float *data);*/

/* Cheker functions*/
void void_check(void* m);
void mat_index_check(Mat_i* m, size_t i, size_t j);
void mat_index_check(Mat_f* m, size_t i, size_t j);
void mat_add_size_check(Mat_i* mat_1, Mat_i* mat_2);
void mat_add_size_check(Mat_f* mat_1, Mat_f* mat_2);
void vec_add_size_check(Vec_i* vec_1, Vec_i* vec_2);
void vec_add_size_check(Vec_f* vec_1, Vec_f* vec_2);
void mat_mul_size_check(Mat_i* mat_1, Mat_i* mat_2);
void mat_mul_size_check(Mat_f* mat_1, Mat_f* mat_2);

/* In case of index-out-of-bounds, the functions should
 * write to stderr and exit the process */

/* it should return a pointer to the memory
 * location holding m[i][j],
 * so it could be read from or written to by
 * the generated program */
int* get_index_matrix(Mat_i* m, size_t i, size_t j);
float* get_index_matrix(Mat_f *m, size_t i, size_t j);
int* get_index_vec(Vec_i* v, size_t i);
float* get_index_vec(Vec_f* v, size_t i);


/* the following are linear algebra operations */
Mat_i* add_mat_mat(Mat_i* mat_1, Mat_i* mat_2);
Mat_f* add_mat_mat(Mat_f* mat_1, Mat_f* mat_2);
Vec_i* add_vec_vec(Vec_i* vec_1, Vec_i* vec_2);
Vec_f* add_vec_vec(Vec_f* vec_1, Vec_f* vec_2);
Mat_i* scalar_mul_mat(int num, Mat_i* mat);
Mat_f* scalar_mul_mat(int num, Mat_f* mat);
Vec_i* scalar_mul_vec(int num, Vec_i* vec);
Vec_f* scalar_mul_vec(int num, Vec_f* vec);
Mat_i* mat_product_int(Mat_i* mat_1, Mat_i* mat_2);
Mat_f* mat_product_int(Mat_f* mat_1, Mat_f* mat_2);

// more operations to follow

/* Matrix type cast*/
Mat_f* mat_int_to_float(Mat_i* mat);
Mat_i* mat_float_to_int(Mat_f* mat);
#endif