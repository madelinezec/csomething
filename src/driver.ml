let rec debug_print_token_list = function
    | [] -> ()
    | token :: tokens -> print_string @@ " " ^ (Lexer.show_token token); debug_print_token_list tokens


let main () = 
    if Array.length Sys.argv != 2 then
        print_endline "Usage: csomething [filename]"
    else
        let filename = Sys.argv.(1) in
        let ast = Parser.default_tokenlist filename |> Parser.parseProgram in
        print_endline @@ Ast.show_program ast;;

Printexc.record_backtrace true;
try
    main ()
with
    Parser.Syntax_error e -> Printexc.print_backtrace stderr; print_newline (); print_endline e;;
