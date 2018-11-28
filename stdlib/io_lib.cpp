#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <math.h>

#include "matrix_lib.hpp"

/* read one integer from stdin */

extern "C" int64_t get_int(){
  char buffer[BUFSIZ];
  read(0, buffer, BUFSIZ);
  int sign = 1;
  int offset = 0;
    //  source for converting string to integer: 
  //  https://www.programmingsimplified.com/c/source-code/c-program-convert-string-to-integer-without-using-atoi-function
  if (buffer[0] == '-') {  // Handle negative integers
    sign = -1;
  }
 
  if (sign == -1) {  // Set starting position to convert
    offset = 1;
  }
  
  else {
    offset = 0;
  }
 
  int n = 0;
 
  for (int c = offset; buffer[c] != '\n'; c++) {
    n = n * 10 + buffer[c] - '0';  
  }
 
  if (sign == -1) {
    n = -n;
  }
  return n;
    
}

/* write one integer to stdout */
extern "C" void put_int(int x){
   /* printf("%ld", x);*/
  char str[BUFSIZ];

  int i = 0;
  int neg_sign = 0; 
    /*source for while loop: https://bit.ly/2TEkpfb */
  if(x < 0){
      neg_sign = 1;
      x = x * -1;
  }

  while (x){ 
    str[i++] = (x%10) + '0';
    x = x/10;
  } 
  str[i] = '\0';
  char revstr[i];
  int len = i;
   
  for(int j = 0; j < len; j++){
    i = i- 1;
    revstr[j] = str[i];
  }

  revstr[len] = '\0';
  if(neg_sign){
    char final_array [len + 1];
	  final_array[0] = '-';
	  for(int i = 0; i < len - 1; i++){
		  final_array[i + 1] = revstr[i];
    }
    final_array[len + 1] = '\0';
    write(1, &final_array, strlen(final_array));
  }
  else{    
    write(1, &revstr, len);
  }

}

/* read one float from stdin */
float get_float(){
  float ret;
  char buffer [BUFSIZ];
  read(0, buffer, strlen(buffer));
  
  //source for converting string to float:
  //https://stackoverflow.com/q/4392665/6637004
  const char* s = buffer;
  float rez = 0, fact = 1;
  if (*s== '-'){
    s++;
    fact = -1;
  };

  for(int point_seen = 0; *s; s++){
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
void put_mat(struct matrix_int* m){
  int rows = m->m;
  int columns = m->n;
  int offset;
  char new_line [] = "\n";
  char white_space [] = " ";

  for(int i = 0; i < rows; i++){
    for(int j = 0; j < columns; j++){
      offset = i * columns + j;
      put_int(*m->data[offset]);
      write(1, white_space, strlen(white_space));
    }
    write(1, new_line, strlen(new_line));
  }
}

void put_mat(struct matrix_float* m){
  int rows = m->m;
  int columns = m->n;
  int offset;
  char new_line [] = "\n";
  char whiteSpace [] = " ";

  for(int i = 0; i < rows; i++){
    for(int j = 0; j < columns; j++){
      offset = i * columns + j;
      put_float(*(m->data[offset]));
      write(1, whiteSpace, strlen(whiteSpace));
    }  
    write(1, new_line, strlen(new_line));
  }
}

/* print a vector */
void put_vec(struct vector_int* v){
  int length = v->n;
  char white_space [] = " ";
  for(int j = 0; j < length; j++){
    put_int(v->data[j]);
    write(1, white_space, strlen(white_space));
  }
}

void put_vec(struct vector_float* v){
  int length = v->n;
  char white_space [] = " ";
  for(int j = 0; j < length; j++){
    put_float(v->data[j]);
    write(1, white_space, strlen(white_space));
  }
}

