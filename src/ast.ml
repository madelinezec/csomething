 (* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

type typ = 
    | Int | Bool | Void | Mat | Vec | Float
    | RealVec of typ * int
    | RealMat of typ * int * int
    [@@deriving show]


type expr =
    Literal of int
  | Number of float
  | BoolLit of bool
  | MatLit of expr list list
  | VecLit of expr list
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of expr * expr
  | Call of string * expr list
  | SingleIndex of expr * expr
  | DoubleIndex of expr * expr * expr
  | Noexpr (*empty*)

type var_decl = {
    vtyp : typ;
    vname : string;
    vvalue : expr option        
}

type stmt =
    Block of stmt list
  | Expr of expr
  | DeclStmt of var_decl
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
    ftyp : typ;
    fname : string;
    formals : var_decl list;
    fbody : stmt list;
}

type decl =
    | VDecl of var_decl
    | FDecl of func_decl

type program = decl list


    (* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"


let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Void -> "void"
  | Mat -> "mat"
  | Float -> "float"
  | RealVec (t, s) -> string_of_typ t ^ "[" ^ string_of_int s ^ "]"
  | RealMat (t, s1, s2) ->
          string_of_typ t ^ "[" ^ string_of_int s1 ^ ", " ^ string_of_int s2 ^ "]"

exception UnexpectedAstEntry of expr

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Number(l) -> string_of_float l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

and string_of_var_decl vd = 
    let decl_Str = string_of_typ vd.vtyp ^ " " ^ vd.vname in
    match vd.vvalue with
        | Some expr -> decl_Str ^ " = " ^ string_of_expr expr ^ ";\n"
        | None -> decl_Str ^ ";\n"

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | DeclStmt(vd) -> string_of_var_decl vd
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s


let string_of_fdecl fdecl =
  string_of_typ fdecl.ftyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_var_decl fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.fbody) ^
  "}\n"

let string_of_decl = function
    | FDecl fd -> string_of_fdecl fd
    | VDecl vd -> string_of_var_decl vd

let string_of_program decls =
  String.concat "" (List.map string_of_decl decls)
