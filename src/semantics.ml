open Ast
open Symbol

type symbol =
    | SymVar of {sv_typ : typ; sv_is_temp : bool; sv_is_global : bool; mutable sv_ref : int }
    | SymFun of {sf_rtyp : typ; sv_args : typ list; mutable sf_ref : int}
    [@@deriving show]

type op = Ast.op

type uop = Ast.uop

type typ = 
    | Int | Bool | Void | Mat | Float | Vec
    [@@deriving show]


type expr =
    Literal of int
  | Number of float
  | BoolLit of bool
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of expr * expr
  | SingleIndex of expr * int
  | DoubleIndex of expr * int * int
  | SymRefVar of symbol
  | Call of symbol * expr list
  | Noexpr

type stmt =
    Block of symbol symbol_table * stmt list
  | Expr of expr
  | DeclStmt of var_decl
  | Return of expr
  | If of expr * stmt * stmt
  | While of expr * stmt

type func_decl = {
    ftyp : typ;
    fname : string;
    formals : var_decl list;
    fbody : symbol symbol_table * stmt list;
}

type var_decl = {
    vtyp : typ;
    vname : string;
    vvalue : expr 
}

type decl =
    | VDecl of var_decl
    | FDecl of func_decl

type program = decl list

exception Unimplemented
exception UnknownIdentifier of string
exception FunctionNameAsId of string
exception CallingAnId of string

let desugar_typ : Ast.typ -> typ = function
    | Ast.Int -> Int
    | Ast.Bool -> Bool
    | Ast.Void -> Void
    | Ast.Mat -> Mat
    | Ast.Float -> Float
    | Ast.Vec _ -> Vec
    | Ast.RealMat _ -> Mat

let rec desugar_expr (st : symbol symbol_table ref) = function
    | Ast.Literal x -> Literal x
    | Ast.Number f -> Number f
    | Ast.BoolLit b -> BoolLit b
    | Ast.Id id -> 
        begin match !st#find id with
            | Some (SymVar sv as sym) -> sv.sv_ref <- sv.sv_ref + 1; SymRefVar sym
            | Some _-> raise (FunctionNameAsId id)
            | None -> raise (UnknownIdentifier id)
        end
    | Ast.Binop (e1, op, e2) -> 
        Binop (desugar_expr st e1, op, desugar_expr st e2)
    | Ast.Unop (op, e) -> Unop (op, desugar_expr st e)
    | Ast.Assign (e1, e2) -> Assign (desugar_expr st e1, desugar_expr st e2)
    | Ast.Call (fn, li) -> let sym =
        begin match !st#find fn with
            | Some (SymFun sf) -> sf.sf_ref <- sf.sf_ref + 1; SymFun sf
            | Some _ -> raise (CallingAnId fn)
            | None -> raise (UnknownIdentifier fn)
        end in
        Call(sym, List.map (desugar_expr st) li)
    | Ast.Noexpr -> Noexpr



let desugar_decl = function
    | Ast.VDecl {vtyp = vtyp; vname = vname; vvalue = vvalue} -> raise Unimplemented
    | Ast.FDecl {ftyp = ftyp; fname = fname; formals = formals; fbody = fbody} -> raise Unimplemented

let rec desugar_program : Ast.program -> program = function
    | [] -> []
    | x :: xs -> desugar_decl x :: desugar_program xs
