
let in_channel = open_in "tests/test.cst"


let _ =   
   let lexbuf = Lexing.from_channel in_channel in
    let ast = Parser.program Scanner.token lexbuf in
    print_string (Ast.string_of_program ast)
