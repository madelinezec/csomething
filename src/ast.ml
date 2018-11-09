   type program =
   |  Decls of typ * identifier * decls_prime

type typ =
   | INT
   | BOOL
   | VOID

type identifier = string

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

(*stmt stmt_list | epsilon*)
type stmtlist =
    | StmtList of stmt * stmtlist
    | StmtlistNil 

(* stmt = “RETURN” stmt_prime| expr SEMI |“LBRACE” stmt_list RBRACE| IF LPAREN expr RPAREN stmt stmt_prime_prime| FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt| WHILE LPAREN expr RPAREN stmt*)
and stmt = 
| Return of expr option
| Expression of expr
| StmtList of stmtlist
| IF of expr * stmt * stmt option
| FOR of expr option * expr * expr option * stmt 
| WHILE of expr * stmt

(*stmt_prime = SEMI| expr SEMI

DELETE 

type stmtprime
| SEMI 
| Expression of expr  *)

(*NOELSE | ELSE stmt
type stmtprimeprime
| NOELSE 
| ELSE of stmt

DELETE
*)

(* Expr_opt = expr | epsilon 
type expropt =
| Expression of expr 
| ExprNil 
DELETE
*)

type Expr

type ExprPrime

(* Actuals_opt  = actuals_list  | epsilon *)
type ActualsOpt= 
| ActualsList of actualslist 
| ActualsNil

type ActualsList = 
| ActualsList of expr * actualslistprime

(*actualslistprime = COMMA expr actuals_list_prime | epsilon*)
type actualslistprime = 
| ActualsListPrime of expr * actualslistprime
| ALPNil

 
