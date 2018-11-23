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

let builder = L.builder context

let symbols : L.llvalue symbol_table = new symbol_table None print_val

let int_t = L.i64_type context
and float_t = L.float_type context
and double_t = L.double_type context
and matrix_t = L.named_struct_type context "mat"
and vector_t = L.named_struct_type context "vec"
and void_t = L.void_type context

