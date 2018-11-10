type typ =
   | Int
   | Bool
   | Void
   | Epsilon

type identifier = string


type formallistprime =
    | FormalListPrime of formalList
    | Fempty

(* typ “ID” formal_list_prime *)
and formalList =
    | FormalList of typ * identifier * formallistprime

(*formals_opt = formal_list | epsilon *)
type formalsOpt =
   | FormalsOpt of formalList
   | []


type variabledeclarationlist =
    | VdeclList of variabledeclarationlist
    | []


(*expr -> T E* *)
type expr = 
| Expression of t * eprime 

and  f = 
| ExpressionParen of expr
| Literal of int 
| BoolLit of bool 
| Id of string

(*T* -> TIMES F T* 
T* -> / F T* 
T* -> ε*)
and tprime = 
| Divide of f * tprime 
| Times of f * tprime
| TEmpty

(*T -> F T*)
and t = 
| F of f * tprime

(*E* -> + T E* 
E* -> - T E* 
E* -> ε  *)
and eprime = 
| Add of t * eprime
| Minus of t * eprime
| Eempty



type assignment = 
| IA of identifier * typ option
| AExpr of expr


type stmtlist =
    | StmtList of stmt * stmtlist
    | StmtlistNil 
(* OR type stmt_list = 
| StatementList of stmt * (stmt list) option ???*)

(*. stmt -> assignment SEMI 
stmt -> RETURN stmt_opt 
stmt -> LBRACE stmt_list RBRACE 
stmt -> IF LPAREN assignment RPAREN stmt 
stmt -> FOR LPAREN assignment SEMI assignment SEMI assignment RPAREN stmt  
stmt -> WHILE LPAREN assignment RPAREN stmt
*)
and stmt = 
| Assignment of identifier * typ option * expr
| Return of expr option
| Parentheses of stmtlist
| If of assignment * stmt
| For of assignment * assignment * assignment * stmt
| While of assignment * stmt
(*“lparen” formals_opt “rparen” “LBRACE” vdecl_list stmt_list “RBRACE”*)
type functiondeclaration =
    | Fdecl of formalsOpt * variabledeclarationlist * stmtlist


type decls = 
| Declaration of typ * identifier * declsprime 
| DEmpty

(* decls_prime = vdecl decls | fdecl decls *)
and declsprime =
   | Vdecl of decls
   | Fdecl of functiondeclaration * decls

type program =
   |  Decls of typ * identifier * declsprime
