PLT FALL 2018



C Something?

Project Proposal


Tong Liu (tl2871) Compiler Architect 

Zhuohao Li (zl2630) Language Guru 

Zixiong Liu (zl2683) System Architect 

Madeline Zechar (mez2113)  Manager 



September 26 2018

















C Something?


Language Description
C Something? is an approach to directly supports linear algebra computation based on a subset of C syntax. Its major strength is concise and simple syntax for linear algebra computation. In C Something?, math operations’ efficiency are enhanced by compiler level optimizations, like parallel computing. Moreover, C Something? supports a form of syntax for math operations styled after natural language, making it an attractive language to math learners with limited programming skills.

A C Something? program consists of a sort of functions, variables, and linear algebra operations, similar to C.

Motivation　
Mathematical libraries are powerful tools for performing numerical calculations. However, with recursive function calls, input arguments, etc programming solutions can become very tedious for even simple computations. C Something? pairs the robust capabilities of mathematical programming with the intuitiveness of natural language. In this way, C Something? syntax aspires to reinforce mathematics as a language.  

Types and Data
C Something? supports basic C-style types, extended to suit our needs to handle math operations. Basic types will include int, char, string, float, fptr (function pointers). For the four major C data types: int is used to store an integer with 4 bytes; float is used to store decimal numbers with single precision with 4 bytes; char is used to store single character with 1 byte; double is used to store decimal numbers with double decision with 8 bytes.

Due to the difficulty of implementation, we currently do not aim for type safety and our language is weakly typed. This design choice allows us to not check for compatibility of function pointers, i.e. we only have one fptr type, which may point to a function of any signature. 
There are also higher order data types (containers), which are types that have to be modified by one or several basic types. These include T[n] (arrays of T) and T[m, n] (an m by n matrix).

For matrix types, we will implement type deduction, giving user the choice to use a generic type name “mat” in place of the full type name T[m, n]. Similarly, for vectors we will provide vec in place of array[n].

We plan to add boundary checking for arrays, and a “size” field so that the size of an array is easily known. Matrices are implemented as two dimensional arrays in row major order.

We do not support pointer types.

Syntax
For basic operators and flow controls, we mostly follow the standard of the C language. 

Arithmetic operators
    Operators (C syntax)
Operators (NL like syntax)
    Meaning
+
add
    Addition
-
minus, min
    Subtraction
*
multiply, mul 
     Multiplication
/
divide, div
     Division
  %
mod
     Remainder of division


Relational operators
    Operators (C syntax)
Operators (NL like syntax)
    Meaning
>
greater than
  greater than
<
smaller than
smaller than
 ==
equal
equals to
 !=
not equal
not equal to
<=
smaller or equal
smaller or equal to
 >=
greater or equal
greater or equal to

Flow control
If-else, else-if, while and for loops are kept in C Something.
Simple Examples Code for if-else statement:
//If-else:
if (n=1) 
   if (a<b)
        x=a
   else 
        x=b

//Else-if:
if (n=1)
   x=a
else if (n=2)
   x=b
else  
   x=0


Arithmetic operators are overloaded for two dimensional arrays to represent matrix arithmetics. The normal +, -, * represent matrix addition, subtraction and multiplication respectively. C Something? also provides a built-in function “inv”, which computes the inverse of a matrix. These operations are implemented as functions in the standard library, and any statements involving these operators will be desugared to corresponding function calls.

Example Codes
Example 1: sample codes for linear algebra. Matrix arithmetic operations.

C Something? code:
//matrix and scalar multiplication, c like syntax
mat a = [2 2 ; 2 2]
int b = 2
mat c = b * a


Equivalent C code:
//matrix and scalar multiplication 
	int a[2][2] = {{2,2},{2,2}};
	int i,j;

	for(i=0; i<2; i++)
    		for(j=0; j<2; j++)
        		a[i][j] = a[i][j] * 2;


As the above example shows, C Something? directly support matrix and linear algebra operations. Using this  language handle math problems can be really concise and straightforward. Moreover, in C Something?, we apply both implicit and explicit compiler level optimizations to math computations. The following examples shows how parallel optimizations can be explicitly applied to the code in C Something?.

Example 2: sample codes for linear algebra. Matrix arithmetic operations, with optimization applied.

C Something? code:
//matrix and scalar multiplication, c like syntax 
mat a = [2 2 ; 2 2]
int b = 2
//set the parallel optimization flag for matrix computation
set_compiler_optimization_flag(PARALLEL_MAT_SCALAR_MUL)
mat c = b * a

Equivalent C code:
//matrix and scalar multiplication 
#include <omp.h>
	int a[2][2] = {{2,2},{2,2}};
	int i,j;

#pragma omp parallel for
	for(i=0; i<2; i++)
		#pragma omp parallel for
    		for(j=0; j<2; j++)
        		a[i][j] = a[i][j] * 2;

C Something? also support a natural language syntax to handle linear algebra operations. This format of language can be greatly user friendly to math learners with limited programming skills. The following examples shows matrix to matrix multiplication with loop unrolling optimizations.

Example 3: sample codes for linear algebra. Matrix multiplication with optimization applied.

C Something? code:
//matrix vector multiplication, natural language like syntax
//natural language like syntax only for basic use cases
//doesn’t support specify optimization flags
matrix a equals [2 2 ; 2 2]
matrix b equals [2 2 ; 2 2]
c is a multiply b
//set the parallel optimization flag for matrix computation
set_compiler_optimization_flag(LOOPUNROLL_MAT_MAT_MUL)

Equivalent C code:
//matrix vector multiplication with loop unrolling optimization
	int a[2][2] = {{2,2},{2,2}};
int b[2][2] = {{2,2},{2,2}};

	int i,j;
	for(i=0; i<2; i++)
    		for(j=0; j<2; j++)
			for(k=0; k<n; k = k + 2){
c[i][j] += a[i][k] * b[k][j];
c[i][j] += a[i][k+1] * b[k+1][j];
}

