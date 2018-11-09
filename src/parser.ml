type token_list = 
  {head : Lexer.token; (** head token. *)
   lexbuf : Lexer.token list}  (** lexer buffer. *)
(** Represents a parser buffer used during parsing of various productions. *)

let default_tokenlist s = {head = Lexer.EOF; lexbuf = Lexer.tokenize s}
(** Create a default [parse_buffer] with the given string [s]. *)

let next tokenlist =
    let {head = _; lexbuf = buf} = tokenlist in
    {head = List.hd buf; lexbuf = List.tl buf}
(** Retrieves a new parser buffer with the next lookahead token. *)

exception SyntaxError of string

(*decls = typ “id” decls_prime | epsilon *)
let parseDecls tokenlist = 
    match tokenlist.lookathead with 
    | "INT" -> match tokenlist.head with 
               | "ID" -> let decls_prime tokenlist_decls_prime = next tokenlist_typ |> parseDeclsPrime in (tokenlist_decls_prime, Ast.Decls("INT", "ID", decls_prime)
               | _ -> raise error
    | "BOOL" -> match tokenlist.head with 
               | "ID" -> let decls_prime tokenlist_decls_prime = next tokenlist_typ |> parseDeclsPrime  in (tokenlist_decls_prime, Ast.Decls("BOOL", "ID", decls_prime)
               | _ -> raise error
    | "INT" -> match tokenlist.head with 
               | "ID" -> let decls_prime tokenlist_decls_prime = next tokenlist_typ |> parseDeclsPrime in (tokenlist_decls_prime, Ast.Decls("VOID", "ID", decls_prime)
               | _ -> raise error
    | "EOF" -> (tokenlist, parseTree)
    | _-> raise error

(* decls_prime = vdecl decls | fdecl decls *)
let parseDeclsPrime tokenlist =
    match tokenlist.head with 
    | "SEMI" -> let vdecl tokenlist_vdecl = next tokenlist |> parseVdecl  in 
                let decls tokenlist_decls = parseDecls tokenlist_vdecl in 
                (tokenlist_decls, Ast.DeclsPrime("SEMI", vdecl, decls)) 
    | "LPAREN" -> let fdecl tokenlist_fdecl = next tokenlist |> parseFdecl in 
                let decls tokenlist_decls = parseDecls tokenlist_fdecl in 
                (tokenlist_decls, Ast.DeclsPrime("SEMI", fdecl, decls)) 
    | _-> raise error

(* Fdecl = “lparen” formals_opt “rparen” “LBRACE” vdecl_list stmt_list “RBRACE” *)
let parseFdecl tokenlist = 
    match tokenlist.head with 
    | "LPAREN" -> let formals_opt tokenlist_formals = next tokenlist |> parseFormals  in 
                  match tokenlist_formals.head with 
                  | "RPAREN" -> let tokenlist_rparen = next tokenlist_formals in
                                match tokenlist_rparen.head with 
                                | "LBRACE" -> let vdecl_list tokenlist_vdecl_list = next tokenlist_formals |> parseVdeclList  in 
                                              let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_vdecl_list in 
                                              match tokenlist_stmt_list.head with 
                                              | "RBRACE" -> (next tokenlist_stmt_list, Ast(something something))
                                              | _-> raise error
                                | _-> raise error
                  | _-> raise error
    | _-> raise error

(* formals_opt = formal_list | epsilon *)
let parseFormalsOpt tokenlist = 
    match tokenlist.head with 
    | "INT" -> let formal_list tokenlist_formal_list = next tokenlist |> parseFormalList in (tokenlist_formal_list, Ast("INT", formal_list))
    | "BOOL" -> let formal_list tokenlist_formal_list = next tokenlist |> parseFormalList in (tokenlist_formal_list, Ast("BOOL", formal_list))
    | "VOID" -> let formal_list tokenlist_formal_list = next tokenlist |> parseFormalList  in (tokenlist_formal_list, Ast("VOID", formal_list))
    | "RPAREN" -> (tokenlist, Ast.Rparen)
    | _-> raise error

(* formal_list = typ “ID” formal_list_prime *)
let parseFormalList tokenlist = 
    match tokenlist.head with 
    | "INT" -> let tokenlist_next = next tokenlist in 
              match tokenlist_next.head with 
               | "ID" -> let tokenlist_typ = next tokenlist_next |> next in 
                         let formal_list_prime tokenlist_formal_list_prime = parseFormalListPrime tokenlist_typ in (tokenlist_formal_list_prime, Ast.Decls("INT", "ID", decls_prime)
               | _ -> raise error
    | "BOOL" -> let tokenlist_next = next tokenlist in
                match tokenlist_next.head with 
               | "ID" -> let tokenlist_typ = next tokenlist_next |> next in 
                         let formal_list_prime tokenlist_formal_list_prime = parseFormalListPrime tokenlist_typ in (tokenlist_formal_list_prime, Ast.Decls("BOOL", "ID", decls_prime)
               | _ -> raise error
    | "VOID" -> let tokenlist_next = next tokenlist in
                match tokenlist_next.head with 
               | "ID" -> let tokenlist_typ = next tokenlist_next |> next in 
                         let formal_list_prime tokenlist_formal_list_prime = parseFormalListPrime tokenlist_typ in (tokenlist_formal_list_prime, Ast.Decls("VOID", "ID", decls_prime)
               | _ -> raise error
    | _-> raise error

(* vdecl_list = vdecl vdecl_list | “epsilon”*)
let rec parseVdeclList tokenlist = 
    match tokenlist.head with 
    | "SEMI" -> let vdecl tokenlist_vdecl =  next tokenlist |> parseVdecl  in 
                let vdecl_list tokenlist_vdecl_list = parseVdeclList tokenlist_vdecl in 
                (tokenlist_vdecl_list, Ast(vdecl, vdecl list))
    | "LPAREN" -> (next tokenlist, Ast.Lparen)
    | "RETURN" -> (next tokenlist, Ast.Lparen)
    | "LBRACE" -> (next tokenlist, Ast.Lparen)
    | "IF" -> (next tokenlist, Ast.If)
    | "FOR" -> (next tokenlist, Ast.For)
    | "WHILE" -> (next tokenlist, Ast.While)
    | "LITERAL" -> (next tokenlist, Ast.Literal)
    | "MINUS" -> (next tokenlist, Ast.Minus)
    | "NOT" -> (next tokenlist, Ast.Not)
    | "ID" -> (next tokenlist, Ast.Lparen)

(* vdecl = “SEMI” *)
(* later we can remove this function*)
let parseVdecl tokenlist = 
    match tokenlist.head with 
    | "SEMI" -> (next tokenlist, Ast.Semi)

(*stmt_list = stmt stmt_list | epsilon*)
let rec parseStmtList tokenlist =
    match tokenlist.head with 
    | "SEMI" -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast("SEMI", stmt, stmt_list))
    | "LPAREN" -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast("LPAREN", stmt, stmt_list))
    | "RETURN" -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast("RETURN", stmt, stmt_list))
    | "LBRACE" -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast("LBRACE", stmt, stmt_list))
    | "RBRACE" -> (tokenlist.next, AST.Rbrace)
    | "IF" -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast("IF", stmt, stmt_list))
    | "FOR" -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast("FOR", stmt, stmt_list))
    | "WHILE" -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast("WHILE", stmt, stmt_list))
    | "LITERAL" -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast("LITERAL", stmt, stmt_list))
    | "MINUS" -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast("MINUS", stmt, stmt_list))
    | "NOT" -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast("NOT", stmt, stmt_list))
    | "ID" -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast("ID", stmt, stmt_list))
    | "TRUE" -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast("TRUE", stmt, stmt_list))
    | "FALSE" -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast("IF", stmt, stmt_list))
   | _-> raise error

(*stmt = “RETURN” stmt_prime| expr SEMI |“LBRACE” stmt_list RBRACE
| IF LPAREN expr RPAREN stmt stmt_prime_prime
| FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
| WHILE LPAREN expr RPAREN stmt*)
let parseStmt tokenlist = 
   match tokenlist.head with 
   | "SEMI" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
               match tokenlist_expr.head with 
               | "SEMI" -> (next tokenlist_expr, Ast.Stmt("SEMI", expr, "SEMI"))
               | _-> raise error 
   | "LPAREN" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
               match tokenlist_expr.head with 
               | "SEMI" -> (next tokenlist_expr, Ast.Stmt("LPAREN", expr, "SEMI"))
               | _-> raise error 
   | "RETURN" -> let stmt_prime tokenlist_stmt_prime = next tokenlist |> parseStmtPrime in 
                 (tokenlist_stmt_prime, Ast.Return(stmt_prime))
   | "LBRACE" -> let stmt_list tokenlist_stmt_list = next tokenlist |> parseStmtList in 
                 match tokenlist_stmt_list.head with 
                 | "RBRACE" -> (next tokenlist_stmt_list, AST.Stmt("LBRACE", stmt_list, "RBRACE"))
   | "IF" -> let tokenlist_lparen = next tokenlist |> next in 
             match tokenlist_lparen with 
             | "LPAREN" -> let expr tokenlist_expr = next tokenlist_lparen |> parseExpr in 
                 match next tokenlist_expr with 
                 | "RPAREN" -> let stmt tokenlist_stmt = next tokenlist_expr |> parseStmt in 
                     let stmt_prime_prime tokenlist_stmt_prime = next tokenlist_stmt |> parseStmtPrime in 
                     (tokenlist_stmt_prime, Ast.Stmt("IF LPAREN", expr, "RPAREN", stmt, stmt_prime_prime)) 
                 | _-> raise error
             | _-> raise error
   | "FOR" -> let tokenlist_lparen = next tokenlist |> next in 
              let expr_opt tokenlist_expr_opt = parseExprOpt tokenlist_lparen in 
              match tokenlist_expr_opt.head with 
              | "SEMI" -> let expr tokenlist_expr = next tokenlist_expr_opt |> parseExpr in
                  match tokenlist_expr.head with 
                  |"SEMI" -> let expr_opt_two tokenlist_expr_opt_two = next tokenlist_expr |> parseExprOpt in 
                      match tokenlist_expr_opt_two.head with 
                      | "RPAREN" -> let stmt tokenlist_stmt = next tokenlist_expr_opt_two |> parseStmt in 
                          (tokenlist_stmt, Ast.Stmt("FOR LPAREN", expr_opt, "SEMI", expr, "SEMI", expr_opt_two, "RPAREN", stmt))
                      | _-> raise error 
                  | _-> raise error
              | _-> raise error
   | "WHILE" -> let tokenlist_lparen = next tokenlist |> next in 
                let expr tokenlist_expr = parseExpr tokenlist_lparen in 
                   match tokenlist_expr with 
                   |"RPAREN" -> let stmt tokenlist_stmt = next tokenlist_expr |> parseStmt in 
                                (tokenlist_stmt, Ast.Stmt("WHILE LPAREN", expr, "RPAREN", stmt))
                   | _-> raise error
   | _-> raise error
(*stmt_prime ->SEMI| expr SEMI*)
let parseStmtPrime tokenlist = 
     match tokenlist.head with 
     | "SEMI" -> (next tokenlist, Ast.Semi)
     | "LPAREN" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                      match tokenlist_expr.head with 
                      |"SEMI" -> (next tokenlist_expr, Ast.StmtPrime("LPAREN", expr, "SEMI")) 
     | "LITERAL" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                      match tokenlist_expr.head with 
                      |"SEMI" -> (next tokenlist_expr, Ast.StmtPrime("LITERAL", expr, "SEMI")) 
     | "MINUS" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                      match tokenlist_expr.head with 
                      |"SEMI" -> (next tokenlist_expr, Ast.StmtPrime("MINUS", expr, "SEMI")) 
     | "NOT" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                      match tokenlist_expr.head with 
                      |"SEMI" -> (next tokenlist_expr, Ast.StmtPrime("NOT", expr, "SEMI")) 
     | "ID" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                      match tokenlist_expr.head with 
                      |"SEMI" -> (next tokenlist_expr, Ast.StmtPrime("ID", expr, "SEMI")) 
     | "TRUE" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                      match tokenlist_expr.head with 
                      |"SEMI" -> (next tokenlist_expr, Ast.StmtPrime("TRUE", expr, "SEMI")) 
     | "FALSE" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                      match tokenlist_expr.head with 
                      |"SEMI" -> (next tokenlist_expr, Ast.StmtPrime("FALSE", expr, "SEMI")) 
     | _ -> raise error  
(*stmt_prime_prime = NOELSE | ELSE stmt*)
let parseStmtPrimePrime tokenlist = 
    match tokenlist.head with 
    | "NOELSE" -> (next tokenlist, Ast.NOELSE))
    |"ELSE" -> (next tokenlist, Ast.ELSE)
    | _-> raise error 

(*Expr_opt = expr | “epsilon”*)
let parseExprOpt tokenlist = 
    match tokenlist.head with 
    |"SEMI" -> (tokenlist, []) 
    | "LPAREN" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  (tokenlist_expr, Ast.ExprOpt("LPAREN", expr))
    |"RPAREN" -> (tokenlist, []) 
    | "LITERAL" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  (tokenlist_expr, Ast.ExprOpt("LITERAL", expr))
    | "MINUS" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  (tokenlist_expr, Ast.ExprOpt("MINUS", expr))
    | "NOT" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  (tokenlist_expr, Ast.ExprOpt("NOT", expr))
    | "ID" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  (tokenlist_expr, Ast.ExprOpt("ID", expr))
    | "TRUE" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  (tokenlist_expr, Ast.ExprOpt("TRUE", expr))
    | "FALSE" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  (tokenlist_expr, Ast.ExprOpt("FALSE", expr))

(* expr = LITERAL expr_prime
       | TRUE expr_prime
       | FALSE expr_prime
       | ID expr_prime
       | MINUS expr %prec NEG expr_prime
       | NOT expr expr_prime
       | ID ASSIGN expr expr_prime
       | ID LPAREN actuals_opt RPAREN expr_prime
       | LPAREN expr RPAREN expr_prime
*)
let parseExpr tokenlist =
    match tokenlist.head with 
    | "LITERAL" -> let expr_prime tokenlist_expr_prime = next tokenlist |> parseExprPrime in 
                   (tokenlist_expr_prime, Ast.Expr("LITERAL", expr_prime))
    | "TRUE" -> let expr_prime tokenlist_expr_prime = next tokenlist |> parseExprPrime in   
                   (tokenlist_expr_prime, Ast.Expr("TRUE", expr_prime))
    | "FALSE" -> let expr_prime tokenlist_expr_prime = next tokenlist |> parseExprPrime in  
                   (tokenlist_expr_prime, Ast.Expr("FALSE", expr_prime))
    | "ID" -> let tokenlist_id = next tokenlist in 
                          match tokenlist_id.head with
                          | "ASSIGN" -> let tokenlist_assign = next tokenlist_id in 
                                         let expr tokenlist_expr = parseExpr tokenlist_assign in 
                                         let expr_prime tokenlist_expr_prime = parseExprPrime tokenlist_expr in
                                         (tokenlist_expr_prime, Ast.Expr("ID ASSIGN", expr, expr_prime))
                          | "LPAREN" ->  let actuals_opt tokenlist_actuals = next tokenlist_id |> parseActualsOpt in 
                                         match tokenlist_actuals.head with
                                         | "RPAREN" -> let expr_prime tokenlist_expr_prime = next tokenlist_actuals |> parseExprPrime in
                                                        (tokenlist_expr_prime, Ast.Expr("ID LPAREN", actuals_opt, "RPAREN", expr_prime))
                                         | _-> raise error
                          | (*how to account for ID expr_prime*)
    | "MINUS" -> let expr tokenlist_expr = next tokenlist |> parseExpr in  
                   match  tokenlist_expr with
                   | "NEG" -> let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                              (tokenlist_expr_prime, Ast.Expr("MINUS", expr, "NEG", expr_prime))
    | "NOT" -> let expr tokenlist_expr = next tokenlist |> parseExpr in
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExpr in
                  (tokenlist_expr_prime, Ast.Expr("NOT", expr, expr_prime))  
    | "LPAREN" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  match tokenlist_expr.head with
                  | "RPAREN" -> let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in
                                (tokenlist_expr_prime, Ast.Expr("LPAREN", expr, "RPAREN", expr_prime))
                  | _-> raise error
    | _-> raise error

(*expr_prime = PLUS expr expr_prime
       | MINUS expr expr_prime
       | TIMES expr expr_prime
       | DIVIDE expr expr_prime
       | EQ expr expr_prime
       | NEQ expr expr_prime
       | LT expr expr_prime
       | LEQ expr expr_prime
       | GT expr expr_prime
       | GEQ expr expr_prime
       | AND expr expr_prime
       | OR expr expr_prime
       | ϵ
*)
let parseExprPrime tokenlist =
  match tokenlist with
   | "PLUS" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                (tokenlist_expr_prime, Ast.ExprPrime("PLUS", expr, expr_prime))
   | "MINUS" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                (tokenlist_expr_prime, Ast.ExprPrime("MINUS", expr, expr_prime))
   | "PLUS" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                (tokenlist_expr_prime, Ast.ExprPrime("PLUS", expr, expr_prime))


   | "TIMES" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                (tokenlist_expr_prime, Ast.ExprPrime("TIMES", expr, expr_prime))
   | "DIVIDE" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                (tokenlist_expr_prime, Ast.ExprPrime("DIVIDE", expr, expr_prime))
   | "EQ" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                (tokenlist_expr_prime, Ast.ExprPrime("EQ", expr, expr_prime))
   | "NEQ" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                (tokenlist_expr_prime, Ast.ExprPrime("NEQ", expr, expr_prime))
   | "LT" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                (tokenlist_expr_prime, Ast.ExprPrime("LT", expr, expr_prime))
  | "LEQ" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                (tokenlist_expr_prime, Ast.ExprPrime("LEQ", expr, expr_prime))
   | "GT" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                (tokenlist_expr_prime, Ast.ExprPrime("GT", expr, expr_prime))
  | "GEQ" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                (tokenlist_expr_prime, Ast.ExprPrime("GEQ", expr, expr_prime))
  | "AND" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                (tokenlist_expr_prime, Ast.ExprPrime("AND", expr, expr_prime))
   | "OR" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                (tokenlist_expr_prime, Ast.ExprPrime("OR", expr, expr_prime))
   | "SEMI" -> (tokenlist, [])
   | "RPAREN" -> (tokenlist, [])
   | "NEG" -> (tokenlist, [])
   | "PLUS" -> (tokenlist, [])
   | "MINUS" -> (tokenlist, [])
   | "TIMES" -> (tokenlist, [])
   | "DIVIDE" -> (tokenlist, [])
   | "EQ" -> (tokenlist, [])
   | "NEQ" -> (tokenlist, [])
   | "LT" -> (tokenlist, [])
   | "LEQ" -> (tokenlist, [])
   | "GT" -> (tokenlist, [])
   | "GEQ" -> (tokenlist, [])
   | "AND" -> (tokenlist, [])
   | "OR" -> (tokenlist, [])
   | "COMMA" -> (tokenlist, [])
   | _ -> raise error  


(*Actuals_opt  = actuals_list  | epsilon*)
let parseActualsOpt tokenlist = 
  match tokenlist.head with
  | "LITERAL" -> let actuals_list tokenlist_actuals = next tokenlist |> parseActualsList in
                  (tokenlist_actuals, Ast.ActualsOpt("LITERAL", actuals_list))
  | "TRUE" -> let actuals_list tokenlist_actuals = next tokenlist |> parseActualsList in
                  (tokenlist_actuals, Ast.ActualsOpt("TRUE", actuals_list))
  | "FALSE" -> let actuals_list tokenlist_actuals = next tokenlist |> parseActualsList in
                  (tokenlist_actuals, Ast.ActualsOpt("FALSE", actuals_list))
  | "ID" -> let actuals_list tokenlist_actuals = next tokenlist |> parseActualsList in
                  (tokenlist_actuals, Ast.ActualsOpt("ID", actuals_list))
  | "MINUS" -> let actuals_list tokenlist_actuals = next tokenlist |> parseActualsList in
                  (tokenlist_actuals, Ast.ActualsOpt("MINUS", actuals_list))
  | "NOT" -> let actuals_list tokenlist_actuals = next tokenlist |> parseActualsList in
                  (tokenlist_actuals, Ast.ActualsOpt("NOT", actuals_list))
  | "LPAREN" -> let actuals_list tokenlist_actuals = next tokenlist |> parseActualsList in
                  (tokenlist_actuals, Ast.ActualsOpt("LPAREN", actuals_list))
  | "RPAREN" -> expr2
  | _ -> raise error

(*actuals_list = expr actuals_list_prime*)
let parseActualsList tokenlist = 
  match value with
  | patt -> expr
  | "LITERAL" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  let actuals_list_prime tokenlist_actuals = parseExpr tokenlist_expr in
  | "TRUE" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  let actuals_list_prime tokenlist_actuals = parseExpr tokenlist_expr in
                  (tokenlist_actuals, Ast.ActualsList("TRUE", expr, actuals_list_prime))
  | "FALSE" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  let actuals_list_prime tokenlist_actuals = parseExpr tokenlist_expr in
                  (tokenlist_actuals, Ast.ActualsList("FALSE", expr, actuals_list_prime))
  | "ID" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  let actuals_list_prime tokenlist_actuals = parseExpr tokenlist_expr in
                  (tokenlist_actuals, Ast.ActualsList("ID", expr, actuals_list_prime))
  | "MINUS" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  let actuals_list_prime tokenlist_actuals = parseExpr tokenlist_expr in
                  (tokenlist_actuals, Ast.ActualsList("MINUS", expr, actuals_list_prime))
  | "NOT" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  let actuals_list_prime tokenlist_actuals = parseExpr tokenlist_expr in
                  (tokenlist_actuals, Ast.ActualsList("NOT", expr, actuals_list_prime))
  | "LPAREN" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  let actuals_list_prime tokenlist_actuals = parseExpr tokenlist_expr in
                  (tokenlist_actuals, Ast.ActualsList("LPAREN", expr, actuals_list_prime))
  | _ -> raise error

(*Actuals_list_prime = COMMA expr actuals_list_prime | epsilon*)
let rec parseActualsListPrime tokenlist = 
  match tokenlist.head with
  | "COMMA" -> let expr tokenlist_expr = next tokenlist |> parseExpr in
                let actuals_list_prime tokenlist_actuals_prime = next tokenlist_expr |> parseExpr in
                (tokenlist_actuals_prime, Ast.ActualsListPrime("COMMA", expr, actuals_list_prime))
  | "RPAREN" -> (tokenlist, [])
  | _ -> raise error

(*Program = decls “EOF”*)
let parseProgram tokenlist = 
    match tokenlist.head with 
      Lexer.Int -> let decls tokenlist_decls = parseDecls tokenlist in 
               let EOF tokenlist_EOF = parseEOF tokenlist_decls in ([], Ast.Program(decls, EOF))
                (* How to return empty token list? *)
    | Lexer.EOF -> let decls tokenlist_decls = parseDecls tokenlist in 
               let Lexer.EOF tokenlist_EOF = parseEOF tokenlist_decls in ([], Ast.Program(decls, EOF))
    | _ -> raise @@ SyntaxError "Int or EOF expected"

