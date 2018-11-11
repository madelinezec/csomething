type typ =
   | Int
   | Bool
   | Void
   | Epsilon
[@@deriving show]


type identifier = string
[@@deriving show]


type formallistprime =
    | FormalListPrime of formalList
    | Fempty
[@@deriving show]

(* typ “ID” formal_list_prime *)
and formalList =
    | FormalList of typ * identifier * formallistprime
[@@deriving show]

(*formals_opt = formal_list | epsilon *)
type formalsOpt =
   | FormalsOpt of formalList
   | FormalsOptEmpty
[@@deriving show]


type variabledeclarationlist =
    | VdeclList of variabledeclarationlist
    | VdeclListEmpty
[@@deriving show]


(*expr -> T E* *)
type expr = 
| Expression of t * eprime 
[@@deriving show]

and  f = 
| ExpressionParen of expr
(* temporary fix *)
| Literal of string 
| BoolLit of bool 
| Id of string
[@@deriving show]

(*T* -> TIMES F T* 
T* -> / F T* 
T* -> ε*)
and tprime = 
| Divide of f * tprime 
| Times of f * tprime
| TPempty
[@@deriving show]

(*T -> F T*)
and t = 
| F of f * tprime
[@@deriving show]

(*E* -> + T E* 
E* -> - T E* 
E* -> ε  *)
and eprime = 
| Add of t * eprime
| Minus of t * eprime
| EPrimeEmpty
[@@deriving show]

type actuals = 
| ActualsList of actuals
| SingleExpression of expr * actuals
| NoActuals
[@@deriving show]
(* assignmentID -> ASSIGN expr | LPAREN actuals RPAREN*)
type assignmentID = 
| MethodCall of actuals
| VariableAssign of expr
[@@deriving show]
(*assignment -> ID assignmentID | typ ID assign expr *)
type assignment = 
| IDAssign of identifier * assignmentID
| TypeAssign of typ * identifier * expr
[@@deriving show]

type stmt_opt = StmtExpression of assignment | OptNil
[@@deriving show]

type stmtlist =
    | StmtList of stmt * stmtlist
    | StmtlistNil 
[@@deriving show]
(* OR type stmt_list = 
| StatementList of stmt * (stmt list) option ???*)
(*stmt_prime ->SEMI| expr SEMI*)

(*. stmt -> assignment SEMI 
stmt -> RETURN stmt_opt 
stmt -> LBRACE stmt_list RBRACE 
stmt -> IF LPAREN assignment RPAREN stmt 
stmt -> FOR LPAREN assignment SEMI assignment SEMI assignment RPAREN stmt  
stmt -> WHILE LPAREN assignment RPAREN stmt
*)
and stmt = 
| Assignment of assignment
| Return of stmt_opt 
| Parentheses of stmtlist
| If of assignment * stmt
| For of assignment * assignment * assignment * stmt
| While of assignment * stmt
(*“lparen” formals_opt “rparen” “LBRACE” vdecl_list stmt_list “RBRACE”*)
[@@deriving show]

type functiondeclaration =
    | Funcdecl of formalsOpt * variabledeclarationlist * stmtlist
[@@deriving show]


type decls = 
| Declaration of typ * identifier * declsprime 
| DEmpty
[@@deriving show]

(* decls_prime = vdecl decls | fdecl decls *)
and declsprime =
   | Vdecl of decls
   | Fdecl of functiondeclaration * decls
[@@deriving show]

type program =
   |  Program of decls 
   | ProgramNil
[@@deriving show]
