#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <math.h>

extern "C" {
#include "matrix_lib.h"
}

/* read one integer from stdin */

extern "C" int64_t get_int(){
    char *p, s [BUFSIZ];
    int n;

    while (fgets(s, sizeof(s), stdin)) {
        n = strtol(s, &p, 10);
        if (p == s || *p != '\n') {
        } else break;
    }
    //printf("You entered: %d\n", n);
    return n;

}

/* write one integer to stdout */
extern "C" void put_int(int x){
       //printf("i get called here");
      	printf("%d", x);

}

extern "C" float get_float(){
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

void put_mat(Mat_i* m){
  int rows = m->m;
  int columns = m->n;
  int offset;
  char new_line [] = "\n";
  char white_space [] = " ";

  for(int i = 0; i < rows; i++){
    for(int j = 0; j < columns; j++){
      put_int(m->data[i][j]);
      printf(" ");
    }
    printf("\n");
  }
}

void put_mat(Mat_f* m){
  int rows = m->m;
  int columns = m->n;
  int offset;

  for(int i = 0; i < rows; i++){
    for(int j = 0; j < columns; j++){
      put_float(m->data[i][j]);
      printf(" ");
    }  
    printf("\n");
  }
}


/* print a vector */
void put_vec(Vec_i* v){
  int length = v->n;
  char white_space [] = " ";
  for(int j = 0; j < length; j++){
    put_int(v->data[j]);
    write(1, white_space, strlen(white_space));
  }
}

void put_vec(Vec_f* v){
  int length = v->n;
  char white_space [] = " ";
  for(int j = 0; j < length; j++){
    put_float(v->data[j]);
    write(1, white_space, strlen(white_space));
  }
}

