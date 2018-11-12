let in_channel = open_in "tests/test.cst"

open Lexing

let print_position outx lexbuf =
    let pos = lexbuf.lex_curr_p in
    Printf.fprintf outx "error near line %d character %d"
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)


let _ =   
    let lexbuf = Lexing.from_channel in_channel in
    try
        let ast = Parser.program Scanner.token lexbuf in
        print_string (Ast.string_of_program ast)
    with
        e -> print_position stderr lexbuf

