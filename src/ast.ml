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
| Add of expr * expr
| Minus of expr * expr
| Times of expr * expr
| Divide of expr * expr
| IntLit of int 
| BoolLit of bool 
| Id of string

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
(*assignment -> ID assignmentID | typ ID assign expr | expression *)
type assignment = 
| IDAssign of identifier * assignmentID
| TypeAssign of typ * identifier * expr
| NoAssign of expr
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
stmt -> RETURN stmt_opt SEMI
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
