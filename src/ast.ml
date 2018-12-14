type typ =
   | Int
   | Bool
   | Void
   | Epsilon
[@@deriving show]


type identifier = string
[@@deriving show]


type bind = typ * string
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
(*temporary fix *)
| IntLit of string 
| BoolLit of bool 
| Id of string
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
(*assignment -> ID assignmentID | typ ID assign expr | expression *)
type assignment = 
| IDAssign of identifier * assignmentID
| TypeAssign of typ * identifier * expr
| NoAssign of expr
[@@deriving show]

type stmt_opt = StmtExpression of assignment | OptNil
[@@deriving show]

type stmt = 
| Block of stmt list 
| Assignment of assignment
| Return of stmt_opt 
| Parentheses of stmt
| If of assignment * stmt
| For of assignment * assignment * assignment * stmt
| While of assignment * stmt
| StmtNil
(*“lparen” formals_opt “rparen” “LBRACE” vdecl_list stmt_list “RBRACE”*)
[@@deriving show]

type functiondeclaration = 
    | Funcdecl of bind list * variabledeclarationlist * stmt list
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
