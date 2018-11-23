open Symbol

type typ = 
    | Int | Bool | Void | Mat | Float | Vec
    [@@deriving show]

type symbol =
    | SymVar of {sv_name : string; sv_typ : typ; sv_is_temp : bool; sv_is_global : bool; mutable sv_ref : int }
    | SymFun of {sf_name : string; sf_rtyp : typ; sf_args : typ list; mutable sf_ref : int}
    [@@deriving show]

type op = Ast.op
[@@deriving show]

type uop = Ast.uop
[@@deriving show]

type expr =
    Literal of int
  | Number of float
  | BoolLit of bool
  | VecLit of expr list
  | MatLit of expr list list
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of expr * expr
  | SingleIndex of expr * expr
  | DoubleIndex of expr * expr * expr
  | SymRefVar of symbol
  | Call of symbol * expr list
  | Noexpr
  [@@deriving show]

type var_decl = {
    vtyp : typ;
    vname : string;
    vvalue : expr 
}
[@@deriving show]

type stmt =
    Block of symbol symbol_table ref * stmt list
  | Expr of expr
  | DeclStmt of var_decl
  | Return of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
[@@deriving show]

type func_decl = {
    ftyp : typ;
    fname : string;
    formals : decl list;
    fbody : symbol symbol_table ref * stmt list;
}
[@@deriving show]

and decl =
    | VDecl of var_decl
    | FDecl of func_decl
    [@@deriving show]

type desugared_program = 
    | Program of decl list * symbol symbol_table ref
[@@driving show]

exception Unimplemented
exception DesugaringBug
exception UnknownIdentifier of string
exception FunctionNameAsId of string
exception CallingAnId of string

let get_formals_type decls =
    let get_decl_type = function
        | FDecl _ -> raise Unimplemented
        | VDecl {vtyp = this_typ} -> this_typ in
    List.map get_decl_type decls

let desugar_for : Ast.stmt -> Ast.stmt = function
    | Ast.For (e1, e2, e3, body) ->
        let loop = Ast.While(e2, Ast.Block (body :: [Ast.Expr e3])) in
        Ast.While(e1, loop)
    | _ -> raise DesugaringBug


let desugar_typ : Ast.typ -> typ = function
    | Ast.Int -> Int
    | Ast.Bool -> Bool
    | Ast.Void -> Void
    | Ast.Mat -> Mat
    | Ast.Vec -> Vec
    | Ast.Float -> Float
    | Ast.RealVec _ -> Vec
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
    | Ast.VecLit xs -> VecLit (List.map (desugar_expr st) xs)
    | Ast.MatLit xs -> MatLit (List.map (function x -> List.map (desugar_expr st) x) xs) 
    | Ast.SingleIndex (v, i) -> SingleIndex ((desugar_expr st v), (desugar_expr st i))
    | Ast.DoubleIndex (v, i, j)
        -> DoubleIndex ((desugar_expr st v), (desugar_expr st i), (desugar_expr st j))

let rec repeat x n =
    if n == 0 then []
    else x :: repeat x (n - 1)

let rec default_value : Ast.typ -> Ast.expr = function
    | Ast.Int -> Ast.Literal 0
    | Ast.Bool -> Ast.BoolLit false
    | Ast.Void -> raise Unimplemented
    | Ast.Float -> Ast.Number 0.0
    | Ast.RealVec (t, len) -> Ast.VecLit (repeat (default_value t) len) 
    | Ast.RealMat (t, l1, l2) -> Ast.MatLit (repeat (repeat (default_value t) l1) l2)
    | _ -> raise Unimplemented

let rec desugar_stmt (st : symbol symbol_table ref) = function
    | Ast.Block li ->
        let new_scope = ref @@ new symbol_table (Some st) show_symbol in
        let li' = List.map (desugar_stmt new_scope) li in
        Block (new_scope, li')
    | Ast.Expr e -> Expr (desugar_expr st e)
    | Ast.DeclStmt vd ->
        let new_symbol =
            SymVar {sv_name = vd.Ast.vname; sv_typ = desugar_typ vd.Ast.vtyp; sv_is_temp = false; sv_is_global = false; sv_ref = 0} in
        !st#add vd.Ast.vname new_symbol;
        let value = 
            begin match vd.Ast.vvalue with
                | Some v ->  v
                | None ->  default_value vd.Ast.vtyp
            end in
        Expr (desugar_expr st (Ast.Assign (Ast.Id vd.Ast.vname, value)))
    | Ast.Return e -> Return (desugar_expr st e)
    | Ast.For _ as f -> desugar_stmt st (desugar_for f)
    | Ast.While (e, b) -> While (desugar_expr st e, desugar_stmt st b)
    | Ast.If (e, s1, s2) -> If (desugar_expr st e, desugar_stmt st s1, desugar_stmt st s2)

let rec desugar_decl st = function
    | Ast.VDecl {Ast.vtyp = vtyp; vname = vname; vvalue = vvalue} ->
        let sym = SymVar {sv_name = vname; sv_typ = desugar_typ vtyp; sv_is_temp = false; sv_is_global = true; sv_ref = 0} in
        !st#add vname sym;
        let new_value = begin match vvalue with
            | Some v -> v
            | None -> default_value vtyp
        end in
        VDecl {vtyp = desugar_typ vtyp; vname = vname; vvalue = desugar_expr st new_value }
    | Ast.FDecl {Ast.ftyp = ftyp; fname = fname; formals = formals; fbody = fbody} ->
        let new_typs = List.map (function vd -> desugar_typ vd.Ast.vtyp) formals in
        let sym = SymFun {sf_name = fname; sf_rtyp = desugar_typ ftyp; sf_args = new_typs; sf_ref = 0} in
        !st#add fname sym;
        let new_ref = ref @@ new symbol_table (Some st) show_symbol in
        let new_formals = List.map (desugar_decl st) (List.map (function x -> Ast.VDecl x) formals) in
        let new_body = List.map (desugar_stmt new_ref) fbody in 
        FDecl {ftyp = desugar_typ ftyp; fname = fname; formals = new_formals; fbody = (new_ref, new_body)}

let rec desugar_program (st: symbol symbol_table ref) : Ast.program -> desugared_program = function
    | [] -> Program ([], st)
    | x :: xs ->
        let decl = desugar_decl st x in
        match desugar_program st xs with
            | Program (res, st) -> Program (decl :: res, st)
