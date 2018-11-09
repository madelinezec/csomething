(*Program = decls “EOF”*)
type Program = 
    | Program of decls

(*decls = typ “id” decls_prime | epsilon *)
type decls = 
   |  Decls of typ * identifier * decls_prime

type typ = 
   | INT 
   | BOOL
   | VOID

(* decls_prime = vdecl decls | fdecl decls *)
type declsprime = 
   | Vdecl of variabledeclaration * decls
   | Fdecl of functiondeclaration * decls

(*“lparen” formals_opt “rparen” “LBRACE” vdecl_list stmt_list “RBRACE”*)
type functiondeclaration =
    | Fdecl of variabledeclarationlist * stmtlist

(*formals_opt = formal_list | epsilon *)
type FormalsOpt =
   |FormalsOpt of formallist

(* typ “ID” formal_list_prime *)
type formalList = 
    | FormalList of typ * identifier * formallistprime

type formallistprime = 
    | FormalListPrime = formalList

type variabledeclarationlist =  
    | VdeclList of variabledeclaration * variabledeclarationlist
 
type StmtList = 
    | StmtList of   

type Stmt

type StmtPrime

type StmtPrimePrime 

type ExprOpt 

type Expr 

type ExprPrime

type ActualsOpt

type ActualsList

type ActualsListPrime 
