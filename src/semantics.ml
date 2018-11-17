open Symbol

type typ = 
    | Int | Bool | Void | Mat | Float | Vec
    [@@deriving show]

type symbol =
    | SymVar of {sv_typ : typ; sv_is_temp : bool; sv_is_global : bool; mutable sv_ref : int }
    | SymFun of {sf_rtyp : typ; sv_args : typ list; mutable sf_ref : int}
    [@@deriving show]

type op = Ast.op

type uop = Ast.uop

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
    Block of symbol symbol_table ref * stmt list
  | Expr of expr
  | DeclStmt of Ast.var_decl
  | Return of expr
  | If of expr * stmt * stmt
  | While of expr * stmt

type func_decl = {
    ftyp : typ;
    fname : string;
    formals : Ast.var_decl list;
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

let default_value : Ast.typ -> Ast.expr = function _ -> raise Unimplemented

let rec desugar_stmt (st : symbol symbol_table ref) = function
    | Ast.Block li ->
        let new_scope = ref @@ new symbol_table (Some st) show_symbol in
        let li' = List.map (desugar_stmt new_scope) li in
        Block (new_scope, li')
    | Ast.Expr e -> Expr (desugar_expr st e)
    | Ast.DeclStmt vd ->
        let new_symbol =
            SymVar {sv_typ = desugar_typ vd.Ast.vtyp; sv_is_temp = false; sv_is_global = false; sv_ref = 0} in
        !st#add vd.Ast.vname new_symbol;
        let value = 
            begin match vd.Ast.vvalue with
                | Some v ->  v
                | None ->  default_value vd.Ast.vtyp
            end in
        Expr (desugar_expr st (Ast.Assign (Ast.Id vd.Ast.vname, value)))
    | Ast.If (e, s1, s2) -> If (desugar_expr st e, desugar_stmt st s1, desugar_stmt st s2) 
    | Ast.While (e, s) -> While (desugar_expr st e, desugar_stmt st s)
        
let desugar_decl = function
    | Ast.VDecl {Ast.vtyp = vtyp; vname = vname; vvalue = vvalue} -> raise Unimplemented
    | Ast.FDecl {Ast.ftyp = ftyp; fname = fname; formals = formals; fbody = fbody} -> raise Unimplemented

let rec desugar_program : Ast.program -> program = function
    | [] -> []
    | x :: xs -> desugar_decl x :: desugar_program xs
