open Semantics
open Symbol
module L = Llvm

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
and matrix_t = L.named_struct_type context "mat"
and vector_t = L.named_struct_type context "vec"
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


let codegen_global_decl st = function
    | VDecl {vtyp = vtyp; vname = vname; vvalue = vvalue} ->
        let init = codegen_const vvalue in
        let sym = L.define_global vname init the_module in
        !st#add vname sym;
        sym
    | FDecl {ftyp = ftyp; fname = fname; formals = formals; fbody = (new_st, stmts)} ->
        let rtyp = get_type ftyp in
        let arg_typ = get_formals_type formals in
        let func_typ = L.function_type rtyp (Array.map get_type (Array.of_list arg_typ)) in
        let sym = L.define_function fname func_typ the_module in
        !st#add fname sym;
        sym

let codegen_program = function
    Program (ds, _) -> 
        let st = ref @@ new symbol_table None print_val in
        ignore @@ List.map (codegen_global_decl st) ds;
        print_endline (L.string_of_llmodule the_module);
