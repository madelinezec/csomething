

let parseProgram tokenlist = 
    
    match tokenlist.head with 
    | "INT" -> let decls tokenlist_decls = parseDecls tokenlist in 
               let EOF tokenlist_EOF = parseEOF tokenlist_decls in (, Ast.Program(decls, EOF)
    | "BOOL" -> let decls tokenlist_decls = parseDecls tokenlist in 
               let EOF tokenlist_EOF = parseEOF tokenlist_decls in Ast.Program(decls, EOF)
    | "VOID" -> let decls tokenlist_decls = parseDecls tokenlist in 
               let EOF tokenlist_EOF = parseEOF tokenlist_decls in Ast.Program(decls, EOF)
    | "EOF" -> let decls tokenlist_decls = parseDecls tokenlist in 
               let EOF tokenlist_EOF = parseEOF tokenlist_decls in Ast.Program(decls, EOF)

let parseDecls tokenlist = 
    match tokenlist.head with 
    | "INT" -> match tokenlist.head with 
               | "ID" -> let decls_prime tokenlist_decls_prime = parseDeclsPrime tokenlist_typ.next in (tokenlist_decls_prime, Ast.Decls("INT", "ID", decls_prime)
               | _ -> raise error
    | "BOOL" -> match tokenlist.head with 
               | "ID" -> let decls_prime tokenlist_decls_prime = parseDeclsPrime tokenlist_typ.next in (tokenlist_decls_prime, Ast.Decls("BOOL", "ID", decls_prime)
               | _ -> raise error
    | "INT" -> match tokenlist.head with 
               | "ID" -> let decls_prime tokenlist_decls_prime = parseDeclsPrime tokenlist_typ.next in (tokenlist_decls_prime, Ast.Decls("VOID", "ID", decls_prime)
               | _ -> raise error
    | "EOF" -> (tokenlist, parseTree)
    | _-> raise error

let parseDeclsPrime tokenlist =
    match tokenlist.head with 
    | "SEMI" -> let vdecl tokenlist_vdecl = parseVdecl tokenlist.next in 
                let decls tokenlist_decls = parseDecls tokenlist_vdecl in 
                (tokenlist_decls, Ast.DeclsPrime("SEMI", vdecl, decls)) 
    | "LPAREN" -> let fdecl tokenlist_fdecl = parseFdecl tokenlist.next in 
                let decls tokenlist_decls = parseDecls tokenlist_fdecl in 
                (tokenlist_decls, Ast.DeclsPrime("SEMI", fdecl, decls)) 
    | _-> raise error

let parseFdecl tokenlist = 
    match tokenlist.head with 
    | "LPAREN" -> let formals_opt tokenlist_formals = parseFormals tokenlist.next in 
                  match tokenlist_formals.head with 
                  | "RPAREN" -> match tokenlist_formals.next with 
                                | "LBRACE" -> let vdecl_list tokenlist_vdecl_list = parseVdeclList tokenlist_formals.next in 
                                              let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_vdecl_list in 
                                              match tokenlist_stmt_list.head with 
                                              | "RBRACE" -> (tokenlist_stmt_list.next, Ast(something something))
                                              | _-> raise error
                                | _-> raise error
                  | _-> raise error
    | _-> raise error

let parseFormalsOpt tokenlist = 
    match tokenlist.head with 
    | "INT" -> let formal_list tokenlist_formal_list = parseFormalList tokenlist.next in (tokenlist_formal_list, Ast("INT", formal_list))
    | "BOOL" -> let formal_list tokenlist_formal_list = parseFormalList tokenlist.next in (tokenlist_formal_list, Ast("BOOL", formal_list))
    | "VOID" -> let formal_list tokenlist_formal_list = parseFormalList tokenlist.next in (tokenlist_formal_list, Ast("VOID", formal_list))
    | "RPAREN" -> (tokenlist, Ast.Rparen)
    | _-> raise error

let parseFormalList tokenlist = 
    match tokenlist.head with 
    | "INT" -> match tokenlist.next with 
               | "ID" -> let tokenlist_typ = next tokenlist |> next in 
                         let formal_list_prime tokenlist_formal_list_prime = parseFormalListPrime tokenlist_typ in (tokenlist_formal_list_prime, Ast.Decls("INT", "ID", decls_prime)
               | _ -> raise error
    | "BOOL" -> match tokenlist.next with 
               | "ID" -> let tokenlist_typ = next tokenlist |> next in 
                         let formal_list_prime tokenlist_formal_list_prime = parseFormalListPrime tokenlist_typ in (tokenlist_formal_list_prime, Ast.Decls("BOOL", "ID", decls_prime)
               | _ -> raise error
    | "VOID" -> match tokenlist.next with 
               | "ID" -> let tokenlist_typ = next tokenlist |> next in 
                         let formal_list_prime tokenlist_formal_list_prime = parseFormalListPrime tokenlist_typ in (tokenlist_formal_list_prime, Ast.Decls("VOID", "ID", decls_prime)
               | _ -> raise error
    | _-> raise error

let rec parseVdeclList tokenlist = 
    match tokenlist.head with 
    | "SEMI" -> let vdecl tokenlist_vdecl = parseVdecl tokenlist.next in 
                let vdecl_list tokenlist_vdecl_list = parseVdeclList tokenlist_vdecl in 
                (tokenlist_vdecl_list, Ast(vdecl, vdecl list))
    | "LPAREN" -> (tokenlist.next, Ast.Lparen)
    | "RETURN" -> (tokenlist.next, Ast.Lparen)
    | "LBRACE" -> (tokenlist.next, Ast.Lparen)
    | "IF" -> (tokenlist.next, Ast.If)
    | "FOR" -> (tokenlist.next, Ast.For)
    | "WHILE" -> (tokenlist.next, Ast.While)
    | "LITERAL" -> (tokenlist.next, Ast.Literal)
    | "MINUS" -> (tokenlist.next, Ast.Minus)
    | "NOT" -> (tokenlist.next, Ast.Not)
    | "ID" -> (tokenlist.next, Ast.Lparen)

let parseVdecl tokenlist = 
    match tokenlist.head with 
    | "SEMI" -> (tokenlist.next, Ast.Semi)

let rec parseStmtList tokenlist =
    match tokenlist.head with 
    | "SEMI" -> let stmt tokenlist_stmt = parseStmt tokenlist.next in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast("SEMI", stmt, stmt_list))
    | "LPAREN" -> let stmt tokenlist_stmt = parseStmt tokenlist.next in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast("LPAREN", stmt, stmt_list))
    | "RETURN" -> let stmt tokenlist_stmt = parseStmt tokenlist.next in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast("RETURN", stmt, stmt_list))
    | "LBRACE" -> let stmt tokenlist_stmt = parseStmt tokenlist.next in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast("LBRACE", stmt, stmt_list))
    | "RBRACE" -> (tokenlist.next, AST.Rbrace)
    | "IF" -> let stmt tokenlist_stmt = parseStmt tokenlist.next in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast("IF", stmt, stmt_list))
    | "FOR" -> let stmt tokenlist_stmt = parseStmt tokenlist.next in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast("FOR", stmt, stmt_list))
    | "WHILE" -> let stmt tokenlist_stmt = parseStmt tokenlist.next in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast("WHILE", stmt, stmt_list))
    | "LITERAL" -> let stmt tokenlist_stmt = parseStmt tokenlist.next in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast("LITERAL"", stmt, stmt_list))
    | "MINUS" -> let stmt tokenlist_stmt = parseStmt tokenlist.next in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast("MINUS", stmt, stmt_list))
    | "NOT" -> let stmt tokenlist_stmt = parseStmt tokenlist.next in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast("NOT", stmt, stmt_list))
    | "ID" -> let stmt tokenlist_stmt = parseStmt tokenlist.next in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast("ID", stmt, stmt_list))
    | "TRUE" -> let stmt tokenlist_stmt = parseStmt tokenlist.next in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast("TRUE", stmt, stmt_list))
    | "FALSE" -> let stmt tokenlist_stmt = parseStmt tokenlist.next in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast("IF", stmt, stmt_list))
   | _-> raise error
let parseStmt tokenlist = 
   match tokenlist.head with 
   | "SEMI" -> let expr tokenlist_expr = parseExpr tokenlist.next in 
               match tokenlist_expr.head with 
               | "SEMI" -> (tokenlist_expr.next, Ast.Stmt("SEMI", expr, "SEMI"))
               | _-> raise error 
   | "LPAREN" -> let expr tokenlist_expr = parseExpr tokenlist.next in 
               match tokenlist_expr.head with 
               | "SEMI" -> (tokenlist_expr.next, Ast.Stmt("LPAREN", expr, "SEMI"))
               | _-> raise error 
   | "RETURN" -> let stmt_prime tokenlist_stmt_prime = parseStmtPrime tokenlist.next in 
                 (tokenlist_stmt_prime, Ast.Return(stmt_prime))
   | "LBRACE" -> let stmt_list tokenlist_stmt_list = parseStmtList tokenlist.next in 
                 match tokenlist_stmt_list.head with 
                 | "RBRACE" -> (tokenlist_stmt_list.next, AST.Stmt("LBRACE", stmt_list, "RBRACE"))
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

let parseStmtPrimePrime tokenlist = 
    match tokenlist.head with 
    | "NOELSE" -> (next tokenlist, Ast.NOELSE))
    |"ELSE" -> (next tokenlist, Ast.ELSE)
    | _-> raise error 

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





let main token_list = 
  let parse_table = Hashtbl.create 123456 in
    Hashtbl.add pase_table ("INT", "program") ["decls", "EOF"]
    Hashtbl.add pase_table ("INT", "decls") ["typ, "ID", "decls_prime"]
    Hashtbl.add pase_table ("INT", "formals_opt") ["formal_list"]
    Hashtbl.add pase_table ("INT", "formal_list") ["typ", "ID", "formal_list_prime"]
    Hashtbl.add pase_table ("INT", "typ") ["INT"]

    Hashtbl.add pase_table ("BOOL", "program") ["decls", "EOF"]
    Hashtbl.add pase_table ("BOOL", "decls") ["typ", "id", "decls_prime"]
    Hashtbl.add pase_table ("BOOL", "formals_opt") ["formal_list"]
    Hashtbl.add pase_table ("BOOL", "formals_list") ["typ", "ID", "formal_list_prime"]
    Hashtbl.add pase_table ("BOOL", "formals_list_prime") ["BOOL"]
    Hashtbl.add pase_table ("BOOL", "typ") ["BOOL"]
    Hashtbl.add pase_table ("VOID", "program") ["decls", "EOF"]
    Hashtbl.add pase_table ("VOID", "decls") ["typ", "ID", "decls_prime"]
    Hashtbl.add pase_table ("VOID", "formals_opt") ["formal_list", "EOF"]
    Hashtbl.add pase_table ("VOID", "formals_list") ["typ", "ID", "formal_list_prime"]
    Hashtbl.add pase_table ("VOID", "typ") ["VOID"]

    Hashtbl.add pase_table ("EOF", "program") ["decls", "EOF"]
    Hashtbl.add pase_table ("EOF", "decls") ["epsilon"]
    Hashtbl.add pase_table ("SEMI", "decls_prime") ["vdecl", "decls"]
    Hashtbl.add pase_table ("SEMI", "vdecl_list") ["vdecl", "vdecl_list"]
    Hashtbl.add pase_table ("SEMI", "vdecl") ["SEMI"]
    Hashtbl.add pase_table ("SEMI", "stmt_list") ["stmt", "stmt_list"]
    Hashtbl.add pase_table ("SEMI", "stmt") ["expr", "SEMI"]
    Hashtbl.add pase_table ("SEMI", "stmt_prime") ["SEMI"]
    Hashtbl.add pase_table ("SEMI", "expr_opt") ["epsilon"]
    Hashtbl.add pase_table ("SEMI", "expr_prim") ["epsilon"]
        
    Hashtbl.add pase_table ("LPAREN", "decls_prime") ["fdecl", "decls"]
    Hashtbl.add pase_table ("LPAREN", "fdecl") ["LPAREN", "formals_opt", "RPAREN", "LBRACE", "vdecl_list", "stmt_list", "RBRACE"]
    Hashtbl.add pase_table ("LPAREN", "vdecl_list") ["epsilon"]
    Hashtbl.add pase_table ("LPAREN", "stmt_list") ["stmt", "stmt_list"]
    Hashtbl.add pase_table ("LPAREN", "stmt") ["expr", "SEMI"]
    Hashtbl.add pase_table ("LPAREN", "stmt_prime") ["expr", "SEMI"]
    Hashtbl.add pase_table ("LPAREN", "expr_opt") ["expr"]
    Hashtbl.add pase_table ("LPAREN", "expr_prime") ["LPAREN", "actuals_opt", "RPAREN"]
    Hashtbl.add pase_table ("LPAREN", "actuals_opt") ["actuals_list"]
    Hashtbl.add pase_table ("LPAREN", "actuals_list") ["expr", "actuals_list_prime"]


    Hashtbl.add pase_table ("RPAREN", "formals_opt") ["epsilon"]
    Hashtbl.add pase_table ("RPAREN", "formals_list_prime") ["epsilon"]
    Hashtbl.add pase_table ("RPAREN", "expr_opt") ["epsilon"]
    Hashtbl.add pase_table ("RPAREN", "expr_prime") ["epsilon"]
    Hashtbl.add pase_table ("RPAREN", "actuals_opt") ["epsilon"]
    Hashtbl.add pase_table ("RPAREN", "actuals_list_prime") ["epsilon"]

    Hashtbl.add pase_table ("COMMA", "formal_list_prime") ["COMMA", "formal_list"]
    Hashtbl.add pase_table ("COMMA", "expr_prime") ["epsilon"]
    Hashtbl.add pase_table ("COMMA", "actuals_list_prime") ["COMMA", "expr", "actuals_list_prime"]

    Hashtbl.add pase_table ("RETURN", "vdecl_list") ["epsilon"]
    Hashtbl.add pase_table ("RETURN", "stmt_list") ["stmt", "stmt_list"]
    Hashtbl.add pase_table ("RETURN", "stmt") ["RETURN", "stmt_prime"]

    
    Hashtbl.add pase_table ("LBRACE", "vdecl_list") ["epsilon"]
    Hashtbl.add pase_table ("LBRACE", "stmt_list") ["stmt", "stmt_list"]
    Hashtbl.add pase_table ("LBRACE", "stmt_list") ["LBRACE", "stmt_list", "RBRACE"]

    Hashtbl.add pase_table ("RBRACE", "vdecl_list") ["epsilon"]

    Hashtbl.add pase_table ("IF", "vdecl_list") ["epsilon"]
    Hashtbl.add pase_table ("IF", "stmt_list") ["stmt", "stmt_list"]
    Hashtbl.add pase_table ("IF", "stmt") ["IF", "LPAREN", "expr", "RPAREN", "stmt", "stmt_prime_prime"]


    Hashtbl.add pase_table ("FOR", "vdecl_list") ["epsilon"]
    Hashtbl.add pase_table ("FOR", "stmt_list") ["stmt", "stmt_list"]
    Hashtbl.add pase_table ("FOR", "stmt") ["FOR", "LPAREN", "expr_opt", "SEMI", "expr", "SEMI", "expr_opt", "RPAREN", "stmt"]

    Hashtbl.add pase_table ("WHILE", "vdecl_list") ["epsilon"]
    Hashtbl.add pase_table ("WHILE", "stmt_list") ["stmt", "stmt_list"]
    Hashtbl.add pase_table ("WHILE", "stmt") ["WHILE", "LPAREN", "expr", "RPAREN", "stmt"]

    Hashtbl.add pase_table ("LITERAL", "vdecl_list") ["epsilon"]
    Hashtbl.add pase_table ("LITERAL", "stmt_list") ["stmt", "stmt_list"]
    Hashtbl.add pase_table ("LITERAL", "stmt") ["expr", "SEMI"]
