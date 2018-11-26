#include "matrix_lib.h"
#include <stdint.h>
#include <unistd.h>

/* read one integer from stdin */
uint64_t get_int(){
	char buffer[BUFSIZ];
	read(0, buffer, BUFSIZ);
	
	/*source for converting string to integer: 
	https://www.programmingsimplified.com/c/source-code/c-program-convert-string-to-integer-without-using-atoi-function*/
	if (buffer[0] == '-') {  // Handle negative integers
    	sign = -1;
  	}
 
  	if (sign == -1) {  // Set starting position to convert
    	offset = 1;
  	}
  	else {
    	offset = 0;
  	}
 
  	n = 0;
 
  	for (c = offset; buffer[c] != '\0'; c++) {
    	n = n * 10 + buffer[c] - '0';
  	}
 
  	if (sign == -1) {
    	n = -n;
  	}
 
  	return n;
}

/* write one integer to stdout */
void put_int(int x){
    char str[BUFSIZ];

	int i = 0; 
    while (x) 
    { 
        str[i] = (x%10) + '0';
        i++;
        x = x/10; 
    } 
    
    char revstr[i];
    int len = i;

    for(int j = 0; j < len; j++){
        i = i- 1;
        revstr[j] = str[i];
    }
    revstr[len] = '\0';
    write(1, revstr, sizeof(revstr));
    
}

/* read one float from stdin */
float get_float(){
	char buffer [BUFSIZ];
	read(0, buffer, strlen(buffer));
	
	/*source for converting string to float:
	https://stackoverflow.com/q/4392665/6637004*/
	const char * s = buffer;
	float rez = 0, fact = 1;
  	if (*s== '-'){
    	s++;
    	fact = -1;
  	};
  	for(int point_seen = 0; buffer; s++){
    	if (*s == '.'){
      		point_seen = 1; 
      		continue;
    	}
    	int d = *s - '0';
    	if (d >= 0 && d <= 9){
      		if (point_seen){
      			fact /= 10.0f;
      		} 
      		rez = rez * 10.0f + (float)d;
  		}
  	return rez * fact;

}

/* write one float to stdout */
void put_float(float x){
 
	double integral;
    float fractional;

    fractional = modf(x, &integral);

	put_int(integral);
	write(1, ".", strlen("."));
	put_int(fractional);
}

/* print a matrix */
void put_mat(struct matrix* m){
	int rows = m->m;
	int columns = m->n;
	int offset;
	char new_line [] = "\n"
	char white_space [] = " "
	int size;
	if(m->type == 0){
		size = sizeof(int);
	}
	if(m->type == 1){
		size = sizeof(float);
	}
	for(int i = 0; i < rows; i++){
		for(int j = 0; j < columns; j++){
			offset = i * num_columns + j;
			if(m->type == 0){
				put_int(&m->data[offset]);
			}
			if(m->type == 1){
				put_float(&m->data[offset]);
			}
			write(1, white_space, strlen(white_space));
		}
		write(1, new_line, strlen(new_line));
	}
}

/* print a vector */
void put_vec(struct vector *v){
	int length = v->n;
	int size;
	if(v->type == 0){
		size = sizeof(int);
	}
	if(v->type == 1){
		size = sizeof(float);
	}
	for(int j = 0; j < length; j++){
		if(v->type == 0){
			put_int(&v->data[j]);
		}
		if(v->type == 1){
			put_float(&v->data[j]);
		}
		write(1, white_space, strlen(white_space));
	}
}

