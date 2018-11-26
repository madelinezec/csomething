#include <stddef.h>
/* these two are opaque types.
 * they should be internal to the implementation
 * our codegen should not concern itself with
 * the layout of these objects */

 /*possible memory layout:*/ 
 struct matrix {
    size_t m, n;
    int type; // 0 = int, 1 = float, 2 = double, etc.
    void *data; // the actual matrix entries on the heap 
 }
 
 struct vector {
    size_t n;
    int type; // 0 = int, 1 = float, 2 = double, etc.
    void *data; // the actual matrix entries on the heap 
 }

struct matrix;
struct vector;

/* allocates an m*n matrix of type int on the heap */
struct matrix *alloc_mat_int(size_t m, size_t n){
    struct matrix * matrix_ptr = malloc(sizeof (struct matrix));
    if(matrix_ptr == NULL){
    	return NULL;
    }
    matrix_ptr->data = malloc(m * n * sizeof(int));
    if(matrix_ptr->data == NULL){
    	free(matrix_ptr);
    	return NULL;
    }
    matrix_ptr->type = 0;
    matrix_ptr->m = m; 
    matrix_ptr->n = n;
    return matrix_ptr
}

/* allocate matrix of floats */
struct matirx *alloc_mat_float(size_t m, size_t n){
    struct matrix * matrix_ptr = malloc(sizeof (struct matrix));
    if(matrix_ptr == NULL){
    	return NULL;
    }
    matrix_ptr->data = malloc(m * n * sizeof(float));
    if(matrix_ptr->data == NULL){
    	free(matrix_ptr);
    	return NULL;
    }
    matrix_ptr->type = 0;
    matrix_ptr->m = m; 
    matrix_ptr->n = n;
    return matrix_ptr
}

/* allocate an array of integers of size n */
struct vector *alloc_vec_int(size_t n){
	struct vector vector_ptr = malloc(sizeof(struct  vector));
	if(vector_ptr == NULL){
		return NULL;
	}
	vector_ptr->data = malloc(n * sizeof(int))
	if(vector_ptr->data == NULL){
		free(vector_ptr);
		return NULL;
	}
	vector_ptr->n = n;
	vector_ptr->type = 0;
}

/* allocate an array of floats */
struct vector *alloc_vec_float(size_t n){
	struct vector vector_ptr = malloc(sizeof(struct  vector));
	if(vector_ptr == NULL){
		return NULL;
	}
	vector_ptr->data = malloc(n * sizeof(float))
	if(vector_ptr->data == NULL){
		free(vector_ptr);
		return NULL;
	}
	vector_ptr->n = n;
	vector_ptr->type = 1;
}

/* this should call whatever destructor necessary and call free */
void free_matrix(struct matrix *m){
    if(m != NULL){
    	free(matrix->data);
    	free(m);
    }
    
}

void free_vector(struct vector *v){
	if(v != NULL){
		free(v->data);
		free(v);
	}
}

/* fill the matrix m with entries from one-dimensional array "data", row-major ordering */
void fill_mat_int(struct matrix *m, int *data){
    num_rows = m->m; 
    num_columns = m->n;
    int offset = 0;
    for(int i = 0; i < num_rows; i++){
    	for(int j = 0; j < num_columns; j++){
    		offset = i * num_columns + j;
    		m->data[offset] = *(data + (i * j));
    	}	
    }
    
}

void fill_mat_float(struct matrix *m, float *data){
	num_rows = m->m; 
    num_columns = m->n;
    int offset = 0;
    for(int i = 0; i < num_rows; i++){
    	for(int j = 0; j < num_columns; j++){
    		offset = i * num_columns + j;
    		m->data[offset] = *(data + (i * j));
    	}	
    }
    
}

void fill_vec_int(struct vector *v, int *data){
	for(int i = 0; i < num_rows; i++){
    	v->data[i] = *(data + i);
    }
}

void fill_vec_float(struct vector *v, float *data){
	for(int i = 0; i < num_rows; i++){
    	v->data[i] = *(data + i);
    }
}

/* In case of index-out-of-bounds, the functions should
 * write to stderr and exit the process */

/* it should return a pointer to the memory
 * location holding m[i][j],
 * so it could be read from or written to by
 * the generated program */
int *get_index_matrix_int(struct matrix *m, size_t i, size_t j){
	if(i > m->m || j > m->n){
		char str[] = "Index out of bounds";
		write(2, str, strlen(str));
		return;
	}
	int offset = i * num_columns + j;
	int * index_pointer; 
	index_pointer = &m->data[offset];
	return index_pointer;
}

/* similar to the previous function */
float *get_index_matrix_float(struct matrix *m, size_t i, size_t j){
	if(i > m->m || j > m->n){
		char str[] = "Index out of bounds";
		write(2, str, strlen(str));
		return;
	}
	int offset = i * num_columns + j;
	int * index_pointer; 
	index_pointer = &m->data[offset];
	return index_pointer;
}

int *get_index_vec_int(struct vector *v, size_t i){
	if(i > m->n){
		char str[] = "Index out of bounds";
		write(2, str, strlen(str));
		return;
	}
	int * index_pointer;
	index_pointer = &v->data[i];
	return index_pointer
}

float *get_index_vec_float(struct vector *v, size_t i){
	if(i > m->n){
		char str[] = "Index out of bounds";
		write(2, str, strlen(str));
		return;
	}
	float * index_pointer;
	index_pointer = &v->data[i];
	return index_pointer
}


/* the following are linear algebra operations */

void addition_mat_int(struct matrix *dest, struct matrix *arg1, struct matrix *arg2){
	num_rows = arg1->m; 
    num_columns = arg1->n;
    int offset = 0;
    for(int i = 0; i < num_rows; i++){
    	for(int j = 0; j < num_columns; j++){
    		offset = i * num_columns + j;
    		dest->data[offset] = arg1->data[offset] + arg2->data[offset];
    	}	
    }
}
void addition_mat_float(struct matrix *dest, struct matrix *arg1, struct matrix *arg2){
	num_rows = arg1->m; 
    num_columns = arg1->n;
    int offset = 0;
    for(int i = 0; i < num_rows; i++){
    	for(int j = 0; j < num_columns; j++){
    		offset = i * num_columns + j;
    		dest->data[offset] = arg1->data[offset] + arg2->data[offset];
    	}
    }	
}
void addition_vec_int(struct vector *dest, struct vector *arg1, struct vector *arg2){
    for(int i = 0; i < num_rows; i++){
    	dest->data[i] = arg1->data[i] + arg2->data[i];
    }
}
void addition_vec_float(struct vector *dest, struct vector *arg1, struct vector *arg2){
    for(int i = 0; i < num_rows; i++){
    	dest->data[i] = arg1->data[i] + arg2->data[i];
    }
}

void scalar_multiplication_mat_int(struct matrix *dest, int arg1, struct matrix *arg2){
	num_rows = arg2->m; 
    num_columns = arg2->n;
    int offset = 0;
    for(int i = 0; i < num_rows; i++){
    	for(int j = 0; j < num_columns; j++){
    		offset = i * num_columns + j;
    		dest->data[offset] = arg2->data[offset] * arg1;
    	}
    }
}
void scalar_multiplication_mat_float(struct matrix *dest, int arg1, struct matrix *arg2){
	num_rows = arg2->m; 
    num_columns = arg2->n;
    int offset = 0;
    for(int i = 0; i < num_rows; i++){
    	for(int j = 0; j < num_columns; j++){
    		offset = i * num_columns + j;
    		dest->data[offset] = arg2->data[offset] * arg1;
    	}
    }
}
void scalar_multiplication_vec_int(struct vector *dest, int arg1, struct vector *arg2){
    for(int i = 0; i < num_rows; i++){
    	dest->data[i] = arg2->data[i] * arg1;
    }
}
void scalar_multiplication_vec_float(struct vector *dest, int arg1, struct vector *arg2){
    for(int i = 0; i < num_rows; i++){
    	dest->data[i] = arg2->data[i] * arg1;
    }
}

void mat_product_int(struct matrix *dest, struct matrix *arg1, struct matrix *arg2){
	num_rows = arg1->m; 
    num_columns = arg1->n;
    int offset = 0;
    for(int i = 0; i < num_rows; i++){
    	for(int j = 0; j < num_columns; j++){
    		offset = i * num_columns + j;
    		dest->data[offset] = arg1->data[offset] * arg2->data[offset];
    	}
    }
}
void mat_product_float(struct matrix *dest, struct matrix *arg1, struct matrix *arg2){
	num_rows = arg1->m; 
    num_columns = arg1->n;
    int offset = 0;
    for(int i = 0; i < num_rows; i++){
    	for(int j = 0; j < num_columns; j++){
    		offset = i * num_columns + j;
    		dest->data[offset] = arg1->data[offset] * arg2->data[offset];
    	}
    }
}

void mat_inv_int(struct matrix *dest, struct matrix *src){
	if(src->m != src->n){
		char str[] = "Non-square matrices are not invertible";
		write(2, str, strlen(str));
		return;
	}
}
void mat_inv_float(struct matrix *dest, struct matrix *src){
	if(src->m != src->n){
		char str[] = "Non-square matrices are not invertible";
		write(2, str, strlen(str));
		return;
	}
}

// more operations to follow

