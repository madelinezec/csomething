open Semantics
open Symbol
module L = Llvm

exception CodegenTODO
exception CodegenUndefinedVar of string
exception CodegenUndefinedFunc of string
exception CodegenNotAnLValue of expr
exception CodegenBug

(* in charge of allocating expression internal temp variables *)
let temp_pool = ref 0

let get_temp () : string =
    let ret = Printf.sprintf "_%d" !temp_pool in
    temp_pool := !temp_pool + 1;
    ret

let clear_temp () = temp_pool := 0

(* reference: https://www.wzdftpd.net/blog/ocaml-llvm-02.html *)
let rec print_type llty =
    let ty = L.classify_type llty in
    match ty with
        | L.TypeKind.Function -> Printf.sprintf "  function\n"
        | L.TypeKind.Pointer  -> Printf.sprintf "  pointer to" ^ print_type (L.element_type llty)
        | _                   -> Printf.sprintf "  other type\n"

let print_val lv =
    let str1 = Printf.sprintf "name: %s," (L.value_name lv) in
    let llty = L.type_of lv in
    let str2 = Printf.sprintf " type %s\n" (L.string_of_lltype llty) in
    str1 ^ str2 ^ print_type llty

let context = L.global_context ()

let the_module = L.create_module context "csomething"

type llvm_symbols = L.llvalue symbol_table

let symbols : llvm_symbols = new symbol_table None print_val

let int_t = L.i64_type context
and bool_t = L.i1_type context
and float_t = L.float_type context
and double_t = L.double_type context
and matrix_t = L.pointer_type (L.named_struct_type context "mat")
and vector_t = L.pointer_type (L.named_struct_type context "vec")
and void_t = L.void_type context

exception CannotGetType of expr

let get_type = function
    | Int -> int_t
    | Bool -> bool_t
    | Void -> void_t
    | Mat -> matrix_t
    | Float -> float_t
    | Vec -> vector_t

let rec get_const_element_type = function
    | Literal _ -> int_t
    | Number _ -> float_t
    | BoolLit _ -> bool_t
    | VecLit (l :: _) -> get_const_element_type l
    | MatLit (l :: _) -> get_const_element_type (VecLit l)
    | x -> raise (CannotGetType x)

exception NotAConst of expr

let rec codegen_const = function
    | Literal i -> L.const_int int_t i
    | Number f -> L.const_float float_t f
    | BoolLit b -> L.const_int bool_t (if b then 1 else 0)
    | VecLit ls ->
        let arr = Array.of_list ls in
        let ll_arr = Array.map codegen_const arr in
        L.const_array (get_const_element_type (VecLit ls)) ll_arr
    | MatLit ls ->
        let flat_list = List.flatten ls in
        codegen_const (VecLit flat_list)
    | x -> raise (NotAConst x)


let codegen_allocate_var builder old_st new_st =
    let allocate_one _ = begin function
        | SymVar {sv_name = name; sv_typ = typ; sv_is_func_arg = is_func_arg } ->
            if not is_func_arg then
                let new_var = L.build_alloca (get_type typ) name builder in
                !new_st#add name new_var
            else
                ()
        | _ -> raise CodegenBug
    end in
    !old_st#map_curr_level allocate_one

let codegen_ptr builder st : expr -> L.llvalue = function
    | SymRefVar (SymVar {sv_name = name }) ->
        begin match !st#find name with
                | None -> raise (CodegenUndefinedVar name)
                | Some v -> v
        end
    | SingleIndex (v, i) -> raise CodegenTODO
    | DoubleIndex (v, i, j) -> raise CodegenTODO
    | _ -> raise CodegenBug

let rec codegen_expr builder st : expr -> L.llvalue = function
    | Literal _ as c -> codegen_const c
    | Number _ as c -> codegen_const c
    | BoolLit _ as c -> codegen_const c
    | VecLit _ as c -> codegen_const c
    | MatLit _ as c -> codegen_const c
    | Binop _ as b -> codegen_binop builder st b
    | Unop _ as u -> codegen_unop builder st u 
    | Assign _ as a -> codegen_assign builder st a 
    | SymRefVar (SymVar _) as ptr ->
        let var = codegen_ptr builder st ptr in  
        L.build_load var (get_temp ()) builder 
    | SymRefVar _ -> raise CodegenBug
    | Call (SymFun {sf_name = name; sf_rtyp = rtyp}, args) ->
        let func = begin match !st#find name with
            | None -> raise (CodegenUndefinedFunc name)
            | Some v -> v
        end in
        let ll_args = Array.of_list (List.map (codegen_expr builder st) args) in
        let temp_name = begin match rtyp with
            | Void -> ""
            | _ -> get_temp ()
        end in
        L.build_call func ll_args temp_name builder
    | _ -> raise CodegenTODO

and codegen_binop builder st = function
    | Binop (expr1, op, expr2) -> 
       let expr1' = codegen_expr builder st expr1
       and expr2' = codegen_expr builder st expr2 in 
       (match op with
       | Ast.Add -> L.build_add
       | Ast.Sub -> L.build_sub
       | Ast.Mult -> L.build_mul
       | Ast.Div -> L.build_sdiv
       | Ast.Equal -> L.build_icmp L.Icmp.Eq
       | Ast.Neq -> L.build_icmp L.Icmp.Ne
       | Ast.Less -> L.build_icmp L.Icmp.Slt
       | Ast.Leq -> L.build_icmp L.Icmp.Sle
       | Ast.Greater -> L.build_icmp L.Icmp.Sgt
       | Ast.Geq -> L.build_icmp L.Icmp.Sge
       | Ast.And -> L.build_and 
       | Ast.Or -> L.build_or
       ) expr1' expr2' (get_temp ()) builder
    | _ -> raise CodegenBug
and codegen_unop builder st = function
    | Unop(op, expr1) -> let expr1' = codegen_expr builder st expr1 in
        (match op with
        | Ast.Neg -> L.build_neg
        | Ast.Not -> L.build_not) expr1' (get_temp ()) builder
    | _ -> raise CodegenBug

and codegen_assign builder st = function
    | Assign (e1, e2) ->
        let var = codegen_ptr builder st e1 in
        let rhs = codegen_expr builder st e2 in
        L.build_store rhs var builder
    | _ -> raise CodegenBug

let rec codegen_stmt builder st = function
    | Block (old_st, stmts) ->
        let new_st = ref @@ new symbol_table (Some st) print_val in
        codegen_allocate_var builder old_st new_st;
        ignore @@ List.map (codegen_stmt builder new_st) stmts
    | Expr expr -> ignore @@ codegen_expr builder st expr
    | Return expr ->
        let expr = codegen_expr builder st expr in
        ignore @@ L.build_ret expr builder
    | If (e, s1, s2) -> 
        let cond = codegen_expr builder st e in
        let zero = L.const_int bool_t 0 in
        let cond_val = L.build_icmp L.Icmp.Ne cond zero "ifcond" builder in
        let start_bb = L.insertion_block builder in
        let func = L.block_parent start_bb in
        let then_bb = L.append_block context "then" func in
        let else_bb = L.append_block context "else" func in
        let merge_bb = L.append_block context "ifcont" func in
        codegen_stmt (L.builder_at_end context then_bb) st s1;
        ignore @@ L.build_br merge_bb (L.builder_at_end context then_bb);
        codegen_stmt (L.builder_at_end context else_bb) st s2;
        ignore @@ L.build_br merge_bb (L.builder_at_end context else_bb);
        ignore @@ L.build_cond_br cond_val then_bb else_bb (L.builder_at_end context start_bb);
        L.position_at_end merge_bb builder
    | While (e, s) ->
        let start_bb = L.insertion_block builder in
        let func = L.block_parent start_bb in
        let cond_bb = L.append_block context "cond_block" func in
        let loop_bb = L.append_block context "loop_body" func in
        let loop_builder = L.builder_at_end context loop_bb in
        codegen_stmt loop_builder st s;
        let end_bb = L.append_block context "end_block" func in
       
        let cond_builder = L.builder_at_end context cond_bb in
        let cond = codegen_expr cond_builder st e in
        let zero = L.const_int bool_t 0 in

        let cond_val_builder = L.builder_at_end context cond_bb in 
        let cond_val = L.build_icmp L.Icmp.Ne cond zero "ifcond" cond_val_builder in
        ignore @@ L.build_cond_br cond_val loop_bb end_bb (L.builder_at_end context cond_bb);
        ignore @@ L.build_br cond_bb builder;
        ignore @@ L.build_br cond_bb (L.builder_at_end context loop_bb);
        L.position_at_end end_bb builder
    | _ -> raise CodegenBug



let codegen_allocate_args builder st func formals =
    let bind ll_param decl =
        let name, typ = begin match decl with
            | VDecl {vname = name; vtyp = typ} -> (name, typ)
            | _ -> raise CodegenBug
        end in
        let arg = L.build_alloca (get_type typ) name builder in
        ignore @@ L.build_store ll_param arg builder;
        !st#add name arg;
    in
    List.iter2 bind (Array.to_list (L.params func)) formals 

let codegen_func_terminator typ builder =
    ignore @@ match typ with
        | Semantics.Void -> L.build_ret_void builder
        | Semantics.Float -> L.build_ret (L.const_float float_t 0.0) builder
        | Semantics.Int -> L.build_ret (L.const_int int_t 0) builder
        | _ -> raise Unimplemented

let codegen_declare_func st name rtyp args =
        let rtyp = get_type rtyp in
        let func_typ = L.function_type rtyp (Array.map get_type (Array.of_list args)) in
        let sym = L.declare_function name func_typ the_module in
        !st#add name sym

let codegen_global_decl st = function
    | VDecl {vtyp = vtyp; vname = vname; vvalue = vvalue} ->
        let init = codegen_const vvalue in
        let sym = L.define_global vname init the_module in
        !st#add vname sym;
        sym
    | FDecl {ftyp = ftyp; fname = fname; formals = formals; fbody = (old_st, stmts)} ->
        let rtyp = get_type ftyp in
        let arg_typ = get_formals_type formals in
        let func_typ = L.function_type rtyp (Array.map get_type (Array.of_list arg_typ)) in
        let sym = L.define_function fname func_typ the_module in
        !st#add fname sym;
        let func_st = ref @@ new symbol_table (Some st) print_val in
        let builder = L.builder_at_end context (L.entry_block sym) in
        clear_temp ();
        codegen_allocate_args builder func_st sym formals;
        codegen_allocate_var builder old_st func_st;
        ignore @@ List.map (codegen_stmt builder func_st) stmts;
        codegen_func_terminator ftyp builder;
        sym

let codegen_global_func_forward old_st new_st =
    let f _ = 
        function
        | SymFun {sf_name = name; sf_rtyp = rtyp; sf_args = args; sf_is_forward = is_forward;} ->
            if is_forward then
                codegen_declare_func new_st name rtyp args
            else 
                ()
        | _ -> ()
    in 
    !old_st#map_curr_level f
        
let codegen_program = function
    Program (ds, old_st) -> 
        let st = ref @@ new symbol_table None print_val in
        codegen_global_func_forward old_st st;
        ignore @@ List.map (codegen_global_decl st) ds;
        print_endline (L.string_of_llmodule the_module);
        L.print_module "out.s" the_module;
