#include <stdio.h>
#include <omp.h>
#include "matrix_lib.h"

#define STD_ERR 2

/* these two are opaque types.
 * they should be internal to the implementation
 * our codegen should not concern itself with
 * the layout of these objects */

 /*possible memory layout:*/
/*
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
*/

/* allocates an m*n matrix of type int on the heap */
Mat_i* alloc_mat_int(size_t m, size_t n){
    Mat_i* matrix_ptr = (Mat_i*)malloc(sizeof (Mat_i));
    if(matrix_ptr == NULL)
    	exit(-1);
    matrix_ptr->type = 0; 
    matrix_ptr->data = (int**)malloc(m * sizeof(int*));
    if(matrix_ptr->data == NULL){
    	free(matrix_ptr);
    	exit(-1);
    }
    
    int i;
    for (i=0; i<m; i++)
        matrix_ptr->data[i] = (int *)malloc(n * sizeof(int));

    
    matrix_ptr->m = m; 
    matrix_ptr->n = n;
    return matrix_ptr;
}

/* allocate matrix of floats */
Mat_f* alloc_mat_float(size_t m, size_t n){
    Mat_f* matrix_ptr = (Mat_f*)malloc(sizeof (Mat_f));
    if(matrix_ptr == NULL)
    	exit(-1);
    matrix_ptr->type = 1;
    
    matrix_ptr->data = (float**)malloc(m * sizeof(float*));
    if(matrix_ptr->data == NULL){
    	free(matrix_ptr);
    	exit(-1);
    }
    
    int i;
    for (i=0; i<m; i++)
        matrix_ptr->data[i] = (float *)malloc(n * sizeof(float));

    
    matrix_ptr->m = m; 
    matrix_ptr->n = n;
    return matrix_ptr;
}

void mat_from_array_int(Mat_i* mat, int* arr, size_t len) {
    if (len != mat->m * mat->n)
        exit(-1);

    for (int i = 0; i < mat->m; i++)
        for (int j = 0; j < mat->n; j++) {
            mat->data[i][j] = arr[i * mat->n + j];
        }
}

void mat_from_array_float(Mat_f* mat, float* arr, size_t len) {
    if (len != mat->m * mat->n)
        exit(-1);
    for (int i = 0; i < mat->m; i++)
        for (int j = 0; j < mat->n; j++)
            mat->data[i][j] = arr[i * mat->n + j];
}

void vec_from_array_int(Vec_i* vec, int* arr, size_t len) {
    if (len != vec->n)
        exit(-1);
    memcpy(vec->data, arr, len * sizeof(int));
}

void vec_from_array_float(Vec_f* vec, float* arr, size_t len) {
    if (len != vec->n)
        exit(-1);
    memcpy(vec->data, arr, len * sizeof(float));
}

/* allocate an array of integers of size n */
Vec_i* alloc_vec_int(size_t n){
	Vec_i* vector_ptr = (Vec_i*)malloc(sizeof(Vec_i));
	if(vector_ptr == NULL){
		exit(-1);
	}
	vector_ptr->data = (int*)malloc(n * sizeof(int));
	if(vector_ptr->data == NULL){
		free(vector_ptr);
		exit(-1);
	}
	vector_ptr->n = n;
    return vector_ptr;
}

/* allocate an array of floats */
Vec_f* alloc_vec_float(size_t n){
	Vec_f* vector_ptr = (Vec_f*)malloc(sizeof(Vec_f));
	if(vector_ptr == NULL){
		exit(-1);
	}
	vector_ptr->data = (float*)malloc(n * sizeof(float));
	if(vector_ptr->data == NULL){
		free(vector_ptr);
		exit(-1);
	}
	vector_ptr->n = n;
    return vector_ptr;
}

/* this should call whatever destructor necessary and call free */
void free_matrix_int(Mat_i* mat){
    if(!mat)
        return;
    
    int i;
    for(i = 0; i < mat->m; i++)
        free(mat->data[i]);
    free(mat->data);
    free(mat);
}

void free_matrix_float(Mat_f* mat){
    if(!mat)
        return;
    
    int i;
    for(i = 0; i < mat->m; i++)
        free(mat->data[i]);
    free(mat->data);
    free(mat);
}

void free_vector_int(Vec_i *v){
	if(v != NULL){
		free(v->data);
		free(v);
	}
}


void free_vector_float(Vec_f *v){
	if(v != NULL){
		free(v->data);
		free(v);
	}
}

/* fill the matrix m with entries from one-dimensional array "data", row-major ordering 
void fill_mat_int(struct matrix *m, int *data){
    int num_rows = m->m; 
    int num_columns = m->n;
    int offset = 0;
    for(int i = 0; i < num_rows; i++){
    	for(int j = 0; j < num_columns; j++){
    		offset = i * num_columns + j;
    		((int*)(m->data))[offset] = *(data + (i * j));
    	}	
    }
    
}


void fill_mat_float(struct matrix *m, float *data){
    int num_rows = m->m; 
    int num_columns = m->n;
    int offset = 0;
    for(int i = 0; i < num_rows; i++){
    	for(int j = 0; j < num_columns; j++){
    		offset = i * num_columns + j;
    		*(((float*)m->data) + offset) = *(data + (i * j));
    	}	
    }
    
}

void fill_vec_int(struct vector *v, int *data){
	for(int i = 0; i < v->n; i++)
    		*((int*)(v->data) + i) = *(data + i);
    
}

void fill_vec_float(struct vector *v, float *data){
	for(int i = 0; i < v->n; i++)
    		*((float*)(v->data) + i) = *(data + i);

}
*/
/* In case of index-out-of-bounds, the functions should
 * write to stderr and exit the process */

/* it should return a pointer to the memory
 * location holding m[i][j],
 * so it could be read from or written to by
 * the generated program */
void void_check(void* m){
    if(!m){
		char str[] = "Matrix or Vector is NULL";
		write(STD_ERR, str, strlen(str));
		exit(-1);
	}
}


void mat_index_check_int(Mat_i* m, size_t i, size_t j){
    if(i > m->m || j > m->n){
		char str[] = "Index out of bounds";
		write(STD_ERR, str, strlen(str));
		exit(-1);
	}
}

void mat_index_check_float(Mat_f* m, size_t i, size_t j){
    if(i > m->m || j > m->n){
		char str[] = "Index out of bounds";
		write(STD_ERR, str, strlen(str));
		exit(-1);
	}
}


void* get_index_matrix(Mat* m, size_t i, size_t j) {
    if (m->type == 0) return get_index_matrix_int((Mat_i*)m, i, j);
    else return get_index_matrix_float((Mat_f*)m, i, j);
}

int* get_index_matrix_int(Mat_i* m, size_t i, size_t j){
    void_check((void*)m);
    mat_index_check_int(m, i, j);
    
	return &m->data[i][j];
}

/* similar to the previous function */
float* get_index_matrix_float(Mat_f *m, size_t i, size_t j){
    void_check((void*)m);
    mat_index_check_float(m, i, j);
    
	return &m->data[i][j];
}


void* get_index_vec(Vec* m, size_t i) {
    if (m->type == 0) return get_index_vec_int((Vec_i*)m, i);
    else return get_index_vec_float((Vec_f*)m, i);
}

int* get_index_vec_int(Vec_i* v, size_t i){
    void_check((void*) v);
    if(i > v->n){
        char str[] = "Index out of bounds";
        write(STD_ERR, str, strlen(str));
        exit(-1);
    }

	return &v->data[i];
}

float* get_index_vec_float(Vec_f* v, size_t i){
    void_check((void*) v);
	if(i > v->n){
		char str[] = "Index out of bounds";
		write(STD_ERR, str, strlen(str));
		exit(-1);
	}
    return &v->data[i];
}


void mat_add_size_check_int(Mat_i* mat_1, Mat_i* mat_2){
    if(mat_1->m != mat_2->m || mat_1->n != mat_2->n){
        char str[] = "Matrix size doesn't match";
		write(STD_ERR, str, strlen(str));
		exit(-1);
    }
}

void mat_add_size_check_float(Mat_f* mat_1, Mat_f* mat_2){
    if(mat_1->m != mat_2->m || mat_1->n != mat_2->n){
        char str[] = "Matrix size doesn't match";
		write(STD_ERR, str, strlen(str));
		exit(-1);
    }
}


/* the following are linear algebra operations */

Mat* add_mat_mat(Mat* m1, Mat* m2) {
    if (m1->type != m2->type) {
        fprintf(stderr, "Type mismatch: add_mat_mat\n");
        exit(-1);
    }
    if (m1->type == 0) {
        return (Mat*)add_mat_mat_int((Mat_i*)m1, (Mat_i*)m2);
    } else {
        return (Mat*)add_mat_mat_float((Mat_f*)m1, (Mat_f*)m2); 
    }
}


Mat* minus_mat_mat(Mat* m1, Mat* m2) {
    if (m1->type != m2->type) {
        fprintf(stderr, "Type mismatch: minus_mat_mat\n");
        exit(-1);
    }
    if (m1->type == 0) {
        return (Mat*)minus_mat_mat_int((Mat_i*)m1, (Mat_i*)m2);
    } else {
        return (Mat*)minus_mat_mat_float((Mat_f*)m1, (Mat_f*)m2); 
    }
}


Mat_i* minus_mat_mat_int(Mat_i* mat_1, Mat_i* mat_2){
    void_check((void*) mat_1);
    void_check((void*) mat_2);
    
    mat_add_size_check_int(mat_1, mat_2);
    
    int num_rows = mat_1->m; 
    int num_columns = mat_1->n;
    
    Mat_i* pt = alloc_mat_int(num_rows, num_columns);

    int i,j;
    #pragma omp parallel private(i,j)
    {
    #pragma omp for 
    for(i = 0; i < num_rows; i++)
    	for(j = 0; j < num_columns; j++)
    		pt->data[i][j] = mat_1->data[i][j] - mat_2->data[i][j];
    }
    
    return pt;
}

Mat_f* minus_mat_mat_float(Mat_f* mat_1, Mat_f* mat_2){
    void_check((void*) mat_1);
    void_check((void*) mat_2);
    
    mat_add_size_check_float(mat_1, mat_2);
    
    int num_rows = mat_1->m; 
    int num_columns = mat_1->n;
    
    Mat_f* pt = alloc_mat_float(num_rows, num_columns);

    int i,j;
    #pragma omp parallel private(i,j)
    {
    #pragma omp for 
    for(i = 0; i < num_rows; i++)
    	for(j = 0; j < num_columns; j++)
    		pt->data[i][j] = mat_1->data[i][j] - mat_2->data[i][j];
    }
    
    return pt;
}




Mat_i* add_mat_mat_int(Mat_i* mat_1, Mat_i* mat_2){
    void_check((void*) mat_1);
    void_check((void*) mat_2);
    
    mat_add_size_check_int(mat_1, mat_2);
    
    int num_rows = mat_1->m; 
    int num_columns = mat_1->n;
    
    Mat_i* pt = alloc_mat_int(num_rows, num_columns);

    int i,j;
    #pragma omp parallel private(i,j)
    {
    #pragma omp for 
    for(i = 0; i < num_rows; i++)
    	for(j = 0; j < num_columns; j++)
    		pt->data[i][j] = mat_1->data[i][j] + mat_2->data[i][j];
    }
    
    return pt;
    		
}

Mat_f* add_mat_mat_float(Mat_f* mat_1, Mat_f* mat_2){
    void_check((void*) mat_1);
    void_check((void*) mat_2);
    
    mat_add_size_check_float(mat_1, mat_2);
    
    int num_rows = mat_1->m; 
    int num_columns = mat_1->n;
    
    Mat_f* pt = alloc_mat_float(num_rows, num_columns);

    int i,j;
    #pragma omp parallel private(i,j)
    {
    #pragma omp for 
    for(i = 0; i < num_rows; i++)
    	for(j = 0; j < num_columns; j++)
    		pt->data[i][j] = mat_1->data[i][j] + mat_2->data[i][j];
    }
    
    return pt;
}

void vec_add_size_check_int(Vec_i* vec_1, Vec_i* vec_2){
    if(vec_1->n != vec_2->n){
        char str[] = "Vector size doesn't match";
		write(STD_ERR, str, strlen(str));
		exit(-1);
    }
}

void vec_add_size_check_float(Vec_f* vec_1, Vec_f* vec_2){
    if(vec_1->n != vec_2->n){
        char str[] = "Vector size doesn't match";
		write(STD_ERR, str, strlen(str));
		exit(-1);
    }
}

Vec* add_vec_vec(Vec* v1, Vec* v2) {
    if (v1->type != v2->type) {
        fprintf(stderr, "Type mismatch: add_mat_mat\n");
        exit(-1);
    }
    if (v1->type == 0) {
        return (Vec*)add_vec_vec_int((Vec_i*)v1, (Vec_i*)v2);
    } else {
        return (Vec*)add_vec_vec_float((Vec_f*)v1, (Vec_f*)v2);
    }
}


Vec* minus_vec_vec(Vec* v1, Vec* v2) {
    if (v1->type != v2->type) {
        fprintf(stderr, "Type mismatch: minus_mat_mat\n");
        exit(-1);
    }
    if (v1->type == 0) {
        return (Vec*)minus_vec_vec_int((Vec_i*)v1, (Vec_i*)v2);
    } else {
        return (Vec*)minus_vec_vec_float((Vec_f*)v1, (Vec_f*)v2);
    }
}


Vec_i* minus_vec_vec_int(Vec_i* vec_1, Vec_i* vec_2){
    void_check((void*) vec_1);
    void_check((void*) vec_2);
    
    vec_add_size_check_int(vec_1, vec_2);
    
    Vec_i* pt = alloc_vec_int(vec_1->n);
    
    int i;
    #pragma omp parallel private(i)
    {
    #pragma omp for 
    for(i = 0; i < vec_1->n; i++)
    	pt->data[i] = vec_1->data[i] - vec_2->data[i];
    }
    return pt;
}


Vec_f* minus_vec_vec_float(Vec_f* vec_1, Vec_f* vec_2){
    void_check((void*) vec_1);
    void_check((void*) vec_2);
    
    vec_add_size_check_float(vec_1, vec_2);
    
    Vec_f* pt = alloc_vec_float(vec_1->n);
    
    int i;
    #pragma omp parallel private(i)
    {
    #pragma omp for 
    for(i = 0; i < vec_1->n; i++)
    	pt->data[i] = vec_1->data[i] - vec_2->data[i];
    }
    return pt;
}



Vec_i* add_vec_vec_int(Vec_i* vec_1, Vec_i* vec_2){
    void_check((void*) vec_1);
    void_check((void*) vec_2);
    
    vec_add_size_check_int(vec_1, vec_2);
    
    Vec_i* pt = alloc_vec_int(vec_1->n);
    
    int i;
    #pragma omp parallel private(i)
    {
    #pragma omp for 
    for(i = 0; i < vec_1->n; i++)
    	pt->data[i] = vec_1->data[i] + vec_2->data[i];
    }
    return pt;
}

Vec_f* add_vec_vec_float(Vec_f* vec_1, Vec_f* vec_2){
    void_check((void*) vec_1);
    void_check((void*) vec_2);
    
    vec_add_size_check_float(vec_1, vec_2);
    
    Vec_f* pt = alloc_vec_float(vec_1->n);
    
    int i;
    
    #pragma omp parallel private(i)
    {
    #pragma omp for 
    for(i = 0; i < vec_1->n; i++)
    	pt->data[i] = vec_1->data[i] + vec_2->data[i];
    }
    return pt;
}

Mat_i* scalar_mul_mat_int(int num, Mat_i* mat){
    void_check((void*) mat);

    int num_rows = mat->m; 
    int num_columns = mat->n;
    
    Mat_i* pt = alloc_mat_int(num_rows, num_columns);
    
    int i,j;
    for(i = 0; i < num_rows; i++)
    	for(j = 0; j < num_columns; j++)
    		pt->data[i][j] = num * mat->data[i][j];
    
    return pt;
}

Mat_f* scalar_mul_mat_float(float num, Mat_f* mat){
    void_check((void*) mat);
    
    int num_rows = mat->m; 
    int num_columns = mat->n;
    
    Mat_f* pt = alloc_mat_float(num_rows, num_columns);
    
    int i,j;
    for(i = 0; i < num_rows; i++)
    	for(j = 0; j < num_columns; j++)
    		pt->data[i][j] = num * mat->data[i][j];
    
    return pt;
}


Vec_i* scalar_mul_vec_int(int num, Vec_i* vec){
    void_check((void*) vec);
    
    int length = vec->n; 
    
    Vec_i* pt = alloc_vec_int(length);
    
    int i;
    for(i = 0; i < length; i++)
        pt->data[i] = num * vec->data[i];
    
    return pt;
}


Vec_f* scalar_mul_vec_float(float num, Vec_f* vec){
    void_check((void*) vec);
    
    int length = vec->n; 
    
    Vec_f* pt = alloc_vec_float(length);
    
    int i;
    for(i = 0; i < length; i++)
        pt->data[i] = num * vec->data[i];
    
    return pt;
}


void mat_mul_size_check_int(Mat_i* mat_1, Mat_i* mat_2){
    if(mat_1->n != mat_2->m){
        char str[] = "Matrix size doesn't match for MUL";
		write(STD_ERR, str, strlen(str));
		exit(-1);
    }
}

void mat_mul_size_check_float(Mat_f* mat_1, Mat_f* mat_2){
    if(mat_1->n != mat_2->m){
        char str[] = "Matrix size doesn't match for MUL";
		write(STD_ERR, str, strlen(str));
		exit(-1);
    }
}

Mat_i* mat_product_int(Mat_i* mat_1, Mat_i* mat_2){
    void_check((void*) mat_1);
    void_check((void*) mat_2);
    
    
    mat_mul_size_check_int(mat_1, mat_2);
    
    int m = mat_1->m;
    int n = mat_2->n;
    
    Mat_i* pt = alloc_mat_int(m, n);
    //mult
    int i,j,k;
    
    #pragma omp parallel private(i,j,k)
    {
    #pragma omp for 
    for(i=0; i<m; i++)
        for(j=0; j<n; j++)
            for(k=0; k < mat_1->n; k++)
                pt->data[i][j] += mat_1->data[i][k] * mat_2->data[k][j];
    }
    return pt;
}

Mat* mat_product(Mat* m1, Mat* m2) {
    if (m1->type != m2->type) {
        fprintf(stderr, "Type mismatch: mat_product\n");
        exit(-1);
    }
    if (m1->type == 0) {
        return (Mat*)mat_product_int((Mat_i*)m1, (Mat_i*)m2);
    } else {
        return (Mat*)mat_product_float((Mat_f*)m1, (Mat_f*)m2);
    }
}

Mat_f* mat_product_float(Mat_f* mat_1, Mat_f* mat_2){
    void_check((void*) mat_1);
    void_check((void*) mat_2);
    
    mat_mul_size_check_float(mat_1, mat_2);
    
    int m = mat_1->m;
    int n = mat_2->n;
    
    Mat_f* pt = alloc_mat_float(m, n);
    //mult
    int i,j,k;
    #pragma omp parallel private(i,j,k)
    {
    #pragma omp for 
    for(i=0; i<m; i++)
        for(j=0; j<n; j++)
            for(k=0; k < mat_1->n; k++)
                pt->data[i][j] += mat_1->data[i][k] * mat_2->data[k][j];
    }
    return pt;
}
// more operations to follow


//Matrix type cast
Mat_f* mat_int_to_float(Mat_i* mat){
    void_check((void*) mat);
    Mat_f* pt = alloc_mat_float(mat->m, mat->n);
    
    int i,j;
    for(i=0; i<mat->m; i++)
        for(j=0; j<mat->n; j++)
            pt->data[i][j] = mat->data[i][j];
    
    return pt;
}

Mat_i* mat_float_to_int(Mat_f* mat){
    void_check((void*) mat);
    Mat_i* pt = alloc_mat_int(mat->m, mat->n);
    
    int i,j;
    for(i=0; i<mat->m; i++)
        for(j=0; j<mat->n; j++)
            pt->data[i][j] = mat->data[i][j];
    
    return pt;
}

Vec_i* vec_copy_int(Vec_i* mat){
    Vec_i* mat_copy = alloc_vec_int(mat -> n);
    int i;
    
    #pragma omp parallel private(i)
    {
    #pragma omp for 
    for(i=0; i<mat -> n; i++)
            mat_copy->data[i] = mat->data[i];
    }
    
    
    return mat_copy;
}

Vec_f* vec_copy_float(Vec_f* mat){
    Vec_f* mat_copy = alloc_vec_float(mat -> n);
    int i;
    
    #pragma omp parallel private(i)
    {
    #pragma omp for 
    for(i=0; i<mat -> n; i++)
            mat_copy->data[i] = mat->data[i];
    }
    
    return mat_copy;
}



Mat_i* mat_copy_int(Mat_i* mat){
    Mat_i* mat_copy = alloc_mat_int(mat -> m, mat -> n);
    int i,j;
    
    
    #pragma omp parallel private(i,j)
    {
    #pragma omp for 
    for(i=0; i<mat -> m; i++)
        for(j=0; j<mat -> n; j++)
            mat_copy->data[i][j] = mat->data[i][j];
    }
    return mat_copy;
}


Mat_f* mat_copy_float(Mat_f* mat){
    Mat_f* mat_copy = alloc_mat_float(mat -> m, mat -> n);
    int i,j;
    
    #pragma omp parallel private(i,j)
    {
    #pragma omp for 
    for(i=0; i<mat -> m; i++)
        for(j=0; j<mat -> n; j++)
            mat_copy->data[i][j] = mat->data[i][j];
    }

        
    return mat_copy;
}

