type token_list = 
  {head : Lexer.token; (** head token. *)
   lexbuf : Lexer.token list}  (** lexer buffer. *)
(** Represents a parser buffer used during parsing of various productions. *)

let default_tokenlist s = {head = Lexer.EOF; lexbuf = Lexer.tokenize s}
(* Create a default [parse_buffer] with the given string [s]. *)

let next tokenlist =
    let {head = _; lexbuf = buf} = tokenlist in
    {head = List.hd buf; lexbuf = List.tl buf}
(** Retrieves a new parser buffer with the next lookahead token. *)

exception SyntaxError of string

let parseTyp tokenlist = 
    match tokenlist.head with 
    | Lexer.Int -> (next tokenlist, Int) 
    | Lexer.Bool -> (next tokenlist, Bool)
    | Lexer.Void -> (tokenlist.next, Void)  
    | Lexer.EOF -> (tokenlist, Epsilon)

(*decls = typ “id” decls_prime | epsilon *)
let parseDecls tokenlist = 
    let typ, tokenlist_typ = parseTyp(tokenlist) in 
    match tokenlist_typ.head with 
    | Lexer.ID identifier -> let decls_prime, tokenlist_decls_prime = next tokenlist_typ |> parseDeclsPrime in 
                  (tokenlist_decls_prime, Ast.Decls(typ, identifier, decls_prime))
    | Lexer.EOF -> (tokenlist, Ast.Epsilon)
    | _-> raise @@ SyntaxError "expecting ID or EOF"

(* decls_prime = vdecl decls | fdecl decls *)
let parseDeclsPrime tokenlist =
    match tokenlist.head with 
    | Lexer.Semicolon -> let tokenlist_vdecl = next tokenlist in
                let decls tokenlist_decls = parseDecls tokenlist_vdecl in 
                (tokenlist_decls, Ast.DeclsPrime(Lexer.Semicolon, vdecl, decls)) 
    | Lexer.LeftParens -> let fdecl tokenlist_fdecl = next tokenlist |> parseFdecl in 
                let decls tokenlist_decls = parseDecls tokenlist_fdecl in 
                (tokenlist_decls, Ast.DeclsPrime(Lexer.Semicolon, fdecl, decls)) 
    | _-> raise error

(* Fdecl = “lparen” formals_opt “rparen” “LBRACE” vdecl_list stmt_list “RBRACE” *)
let parseFdecl tokenlist = 
    match tokenlist.head with 
    | Lexer.LeftParens -> let formals_opt tokenlist_formals = next tokenlist |> parseFormals  in 
                  match tokenlist_formals.head with 
                  | Lexer.RightParens -> let tokenlist_rparen = next tokenlist_formals in
                                match tokenlist_rparen.head with 
                                | Lexer.LeftBrace -> let vdecl_list tokenlist_vdecl_list = next tokenlist_formals |> parseVdeclList  in 
                                              let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_vdecl_list in 
                                              match tokenlist_stmt_list.head with 
                                              | Lexer.RightBrace -> (next tokenlist_stmt_list, Ast.Fdecl(Lexer.LeftParens, formals_opt, Lexer.RightParens, Lexer.LeftBrace, vdecl_list, stmt_list, Lexer.RightBrace))
                                              | _-> raise error
                                | _-> raise error
                  | _-> raise error
    | _-> raise error

(* formals_opt = formal_list | epsilon *)
let parseFormalsOpt tokenlist = 
    match tokenlist.head with 
    | Lexer.Int -> let formal_list tokenlist_formal_list = next tokenlist |> parseFormalList in (tokenlist_formal_list, Ast.FormalsOpt("INT", formal_list))
    | Lexer.Bool -> let formal_list tokenlist_formal_list = next tokenlist |> parseFormalList in (tokenlist_formal_list, Ast.FormalsOpt(Lexer.Bool, formal_list))
    | Lexer.Void -> let formal_list tokenlist_formal_list = next tokenlist |> parseFormalList  in (tokenlist_formal_list, Ast.FormalsOpt(Lexer.Void, formal_list))
    | Lexer.RightParens -> (tokenlist, [])
    | _-> raise error

(* formal_list = typ “ID” formal_list_prime *)
let parseFormalList tokenlist = 
    match tokenlist.head with 
    | Lexer.Int -> let tokenlist_next = next tokenlist in 
              match tokenlist_next.head with 
               | Lexer.ID identifier -> let tokenlist_typ = next tokenlist_next |> next in 
                         let formal_list_prime tokenlist_formal_list_prime = parseFormalListPrime tokenlist_typ in (tokenlist_formal_list_prime, Ast.FormalList("INT", Lexer.ID identifier, decls_prime))
               | _ -> raise error
    | Lexer.Bool -> let tokenlist_next = next tokenlist in
                match tokenlist_next.head with 
               | Lexer.ID identifier -> let tokenlist_typ = next tokenlist_next |> next in 
                         let formal_list_prime tokenlist_formal_list_prime = parseFormalListPrime tokenlist_typ in (tokenlist_formal_list_prime, Ast.FormalList(Lexer.Bool, Lexer.ID identifier, decls_prime))
               | _ -> raise error
    | Lexer.Void -> let tokenlist_next = next tokenlist in
                match tokenlist_next.head with 
               | Lexer.ID identifier -> let tokenlist_typ = next tokenlist_next |> next in 
                         let formal_list_prime tokenlist_formal_list_prime = parseFormalListPrime tokenlist_typ in (tokenlist_formal_list_prime, Ast.FormalList(Lexer.Void, Lexer.ID identifier, decls_prime))
               | _ -> raise error
    | _-> raise error

(* vdecl_list = vdecl vdecl_list | “epsilon”*)
let rec parseVdeclList tokenlist = 
    match tokenlist.head with 
    | Lexer.Semicolon -> let tokenlist_vdecl = next tokenlist in
                let vdecl_list tokenlist_vdecl_list = parseVdeclList tokenlist_vdecl in 
                (tokenlist_vdecl_list, Ast.VdeclList(Lexer.Semicolon, vdecl list))
    | Lexer.LeftParens -> (next tokenlist, [])
    | Lexer.Return -> (next tokenlist, [])
    | Lexer.LeftBrace -> (next tokenlist, [])
    | Lexer.If -> (next tokenlist, [])
    | Lexer.For -> (next tokenlist, [])
    | Lexer.While -> (next tokenlist, [])
    | Lexer.Numeral str -> (next tokenlist, [])
    | Lexer.Minus -> (next tokenlist, [])
    | Lexer.Not -> (next tokenlist, [])
    | Lexer.ID identifier -> (next tokenlist, Ast.Lparen)

(* vdecl = “SEMI” *)
(* later we can remove this function*)
let parseVdecl tokenlist = 
    match tokenlist.head with 
    | Lexer.Semicolon -> (next tok[][])

(*stmt_list = stmt stmt_list | epsilon*)
let rec parseStmtList tokenlist =
    match tokenlist.head with 
    | Lexer.Semicolon -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList(Lexer.Semicolon, stmt, stmt_list))
    | Lexer.LeftParens -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList(Lexer.LeftParens, stmt, stmt_list))
    | Lexer.Return -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.RETURN(stmt, stmt_list))
    | Lexer.LeftBrace -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList(Lexer.LeftBrace, stmt, stmt_list))
    | Lexer.RightBrace -> (tokenlist.next, AST.Rbrace)
    | Lexer.If -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.IF(stmt, stmt_list))
    | Lexer.For -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.FOR( stmt, stmt_list))
    | Lexer.While -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.WHILE( stmt, stmt_list))
    | Lexer.Numeral str -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.LITERAL(stmt, stmt_list))
    | Lexer.Minus -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.MinuMINUS(stmt, stmt_list))
    | Lexer.Not -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList(Lexer.Not, stmt, stmt_list))
    | Lexer.ID identifier -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList(Lexer.ID identifier, stmt, stmt_list))
    | Lexer.True -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList(Lexer.True, stmt, stmt_list))
    | Lexer.False -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList(Lexer.If, stmt, stmt_list))
   | _-> raise error

(*stmt = “RETURN” stmt_prime| expr SEMI |“LBRACE” stmt_list RBRACE
| IF LPAREN expr RPAREN stmt stmt_prime_prime
| FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
| WHILE LPAREN expr RPAREN stmt*)
let parseStmt tokenlist = 
   match tokenlist.head with 
   | Lexer.Semicolon -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
               match tokenlist_expr.head with 
               | Lexer.Semicolon -> (next tokenlist_expr, Ast.Expression(expr))
               | _-> raise error 
   | Lexer.LeftParens -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
               match tokenlist_expr.head with 
               | Lexer.Semicolon -> (next tokenlist_expr, Ast.Expression(expr))
               | _-> raise error 
   | Lexer.Return -> let stmt_prime tokenlist_stmt_prime = next tokenlist |> parseStmtPrime in 
                 (tokenlist_stmt_prime, Ast.Return(stmt_prime))
   | Lexer.LeftBrace -> let stmt_list tokenlist_stmt_list = next tokenlist |> parseStmtList in 
                 match tokenlist_stmt_list.head with 
                 | Lexer.RightBrace -> (next tokenlist_stmt_list, AST.StmtList(stmt_list))
   | Lexer.If -> let tokenlist_lparen = next tokenlist |> next in 
             match tokenlist_lparen with 
             | Lexer.LeftParens -> let expr tokenlist_expr = next tokenlist_lparen |> parseExpr in 
                 match next tokenlist_expr with 
                 | Lexer.RightParens -> let stmt tokenlist_stmt = next tokenlist_expr |> parseStmt in 
                     let stmt_prime_prime tokenlist_stmt_prime = next tokenlist_stmt |> parseStmtPrime in 
                     (tokenlist_stmt_prime, Ast.Stmt( expr, Lexer.RightParens, stmt, stmt_prime_prime)) 
                 | _-> raise error
             | _-> raise error
   | Lexer.For -> let tokenlist_lparen = next tokenlist |> next in 
              let expr_opt tokenlist_expr_opt = parseExprOpt tokenlist_lparen in 
              match tokenlist_expr_opt.head with 
              | Lexer.Semicolon -> let expr tokenlist_expr = next tokenlist_expr_opt |> parseExpr in
                  match tokenlist_expr.head with 
                  |Lexer.Semicolon -> let expr_opt_two tokenlist_expr_opt_two = next tokenlist_expr |> parseExprOpt in 
                      match tokenlist_expr_opt_two.head with 
                      | Lexer.RightParens -> let stmt tokenlist_stmt = next tokenlist_expr_opt_two |> parseStmt in 
                          (tokenlist_stmt, Ast.Stmt("FOR LPAREN", expr_opt, Lexer.Semicolon, expr, Lexer.Semicolon, expr_opt_two, Lexer.RightParens, stmt))
                      | _-> raise error 
                  | _-> raise error
              | _-> raise error
   | Lexer.While -> let tokenlist_lparen = next tokenlist |> next in 
                let expr tokenlist_expr = parseExpr tokenlist_lparen in 
                   match tokenlist_expr with 
                   |Lexer.RightParens -> let stmt tokenlist_stmt = next tokenlist_expr |> parseStmt in 
                                (tokenlist_stmt, Ast.Stmt("WHILE LPAREN", expr, Lexer.RightParens, stmt))
                   | _-> raise error
   | _-> raise error
(*stmt_prime ->SEMI| expr SEMI*)
let parseStmtPrime tokenlist = 
     match tokenlist.head with 
     | Lexer.Semicolon -> (next tokenlist, Ast.Semi)
     | Lexer.LeftParens -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                      match tokenlist_expr.head with 
                      |Lexer.Semicolon -> (next tokenlist_expr, Ast.StmtPrime(Lexer.LeftParens, expr, Lexer.Semicolon)) 
     | Lexer.Numeral str -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                      match tokenlist_expr.head with 
                      |Lexer.Semicolon -> (next tokenlist_expr, Ast.StmtPrime(Lexer.Numeral str, expr, Lexer.Semicolon)) 
     | Lexer.Minus -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                      match tokenlist_expr.head with 
                      |Lexer.Semicolon -> (next tokenlist_expr, Ast.StmtPrime(Lexer.Minus, expr, Lexer.Semicolon)) 
     | Lexer.Not -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                      match tokenlist_expr.head with 
                      |Lexer.Semicolon -> (next tokenlist_expr, Ast.StmtPrime(Lexer.Not, expr, Lexer.Semicolon)) 
     | Lexer.ID identifier -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                      match tokenlist_expr.head with 
                      |Lexer.Semicolon -> (next tokenlist_expr, Ast.StmtPrime(Lexer.ID identifier, expr, Lexer.Semicolon)) 
     | Lexer.True -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                      match tokenlist_expr.head with 
                      |Lexer.Semicolon -> (next tokenlist_expr, Ast.StmtPrime(Lexer.True, expr, Lexer.Semicolon)) 
     | Lexer.False -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                      match tokenlist_expr.head with 
                      |Lexer.Semicolon -> (next tokenlist_expr, Ast.StmtPrime(Lexer.False, expr, Lexer.Semicolon)) 
     | _ -> raise error  
(*stmt_prime_prime = NOELSE | ELSE stmt*)
let parseStmtPrimePrime tokenlist = 
    match tokenlist.head with 
    | "NOELSE" -> (next tokenlist, Ast.NOELSE)
    |Lexer.Else -> (next tokenlist, Ast.ELSE)
    | _-> raise error 

(*Expr_opt = expr | “epsilon”*)
let parseExprOpt tokenlist = 
    match tokenlist.head with 
    |Lexer.Semicolon -> (tokenlist, []) 
    | Lexer.LeftParens -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  (tokenlist_expr, Ast.ExprOpt(Lexer.LeftParens, expr))
    |Lexer.RightParens -> (tokenlist, []) 
    | Lexer.Numeral str -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  (tokenlist_expr, Ast.ExprOpt(Lexer.Numeral str, expr))
    | Lexer.Minus -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  (tokenlist_expr, Ast.ExprOpt(Lexer.Minus, expr))
    | Lexer.Not -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  (tokenlist_expr, Ast.ExprOpt(Lexer.Not, expr))
    | Lexer.ID identifier -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  (tokenlist_expr, Ast.ExprOpt(Lexer.ID identifier, expr))
    | Lexer.True -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  (tokenlist_expr, Ast.ExprOpt(Lexer.True, expr))
    | Lexer.False -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  (tokenlist_expr, Ast.ExprOpt(Lexer.False, expr))

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
    | Lexer.Numeral str -> let expr_prime tokenlist_expr_prime = next tokenlist |> parseExprPrime in 
                   (tokenlist_expr_prime, Ast.Expr(Lexer.Numeral str, expr_prime))
    | Lexer.True -> let expr_prime tokenlist_expr_prime = next tokenlist |> parseExprPrime in   
                   (tokenlist_expr_prime, Ast.Expr(Lexer.True, expr_prime))
    | Lexer.False -> let expr_prime tokenlist_expr_prime = next tokenlist |> parseExprPrime in  
                   (tokenlist_expr_prime, Ast.Expr(Lexer.False, expr_prime))
    | Lexer.ID identifier -> let tokenlist_id = next tokenlist in 
                          match tokenlist_id.head with
                          | Lexer.Assignment -> let tokenlist_assign = next tokenlist_id in 
                                         let expr tokenlist_expr = parseExpr tokenlist_assign in 
                                         let expr_prime tokenlist_expr_prime = parseExprPrime tokenlist_expr in
                                         (tokenlist_expr_prime, Ast.Expr("ID ASSIGN", expr, expr_prime))
                          | Lexer.LeftParens ->  let actuals_opt tokenlist_actuals = next tokenlist_id |> parseActualsOpt in 
                                         match tokenlist_actuals.head with
                                         | Lexer.RightParens -> let expr_prime tokenlist_expr_prime = next tokenlist_actuals |> parseExprPrime in
                                                        (tokenlist_expr_prime, Ast.Expr("ID LPAREN", actuals_opt, Lexer.RightParens, expr_prime))
                                         | _-> raise error
                         (*  | (*how to account for ID expr_prime*)*)
    | Lexer.Minus -> let expr tokenlist_expr = next tokenlist |> parseExpr in  
                   match  tokenlist_expr with
                   | Lexer.Neg -> let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                              (tokenlist_expr_prime, Ast.Expr(Lexer.Minus, expr, Lexer.Neg, expr_prime))
    | Lexer.Not -> let expr tokenlist_expr = next tokenlist |> parseExpr in
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExpr in
                  (tokenlist_expr_prime, Ast.Expr(Lexer.Not, expr, expr_prime))  
    | Lexer.LeftParens -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  match tokenlist_expr.head with
                  | Lexer.RightParens -> let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in
                                (tokenlist_expr_prime, Ast.Expr(Lexer.LeftParens, expr, Lexer.RightParens, expr_prime))
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
   | Lexer.Plus -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                (tokenlist_expr_prime, Ast.ExprPrime(Lexer.Plus, expr, expr_prime))
   | Lexer.Minus -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                (tokenlist_expr_prime, Ast.ExprPrime(Lexer.Minus, expr, expr_prime))
   | Lexer.Plus -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                (tokenlist_expr_prime, Ast.ExprPrime(Lexer.Plus, expr, expr_prime))


   | Lexer.Multiply -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                (tokenlist_expr_prime, Ast.ExprPrime(Lexer.Multiply, expr, expr_prime))
   | Lexer.Divide -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                (tokenlist_expr_prime, Ast.ExprPrime(Lexer.Divide, expr, expr_prime))
   | Lexer.Equality -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                (tokenlist_expr_prime, Ast.ExprPrime(Lexer.Equality, expr, expr_prime))
   | Lexer.Neq -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                (tokenlist_expr_prime, Ast.ExprPrime(Lexer.Neq, expr, expr_prime))
   | Lexer.Less -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                (tokenlist_expr_prime, Ast.ExprPrime(Lexer.Less, expr, expr_prime))
  | Lexer.Leq -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                (tokenlist_expr_prime, Ast.ExprPrime(Lexer.Leq, expr, expr_prime))
   | Lexer.Greater -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                (tokenlist_expr_prime, Ast.ExprPrime(Lexer.Greater, expr, expr_prime))
  | Lexer.Geq -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                (tokenlist_expr_prime, Ast.ExprPrime(Lexer.Geq, expr, expr_prime))
  | Lexer.And -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                (tokenlist_expr_prime, Ast.ExprPrime(Lexer.And, expr, expr_prime))
   | Lexer.Or -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                let expr_prime tokenlist_expr_prime = next tokenlist_expr |> parseExprPrime in 
                (tokenlist_expr_prime, Ast.ExprPrime(Lexer.Or, expr, expr_prime))
   | Lexer.Semicolon -> (tokenlist, [])
   | Lexer.RightParens -> (tokenlist, [])
   | Lexer.Neg -> (tokenlist, [])
   | Lexer.Plus -> (tokenlist, [])
   | Lexer.Minus -> (tokenlist, [])
   | Lexer.Multiply -> (tokenlist, [])
   | Lexer.Divide -> (tokenlist, [])
   | Lexer.Equality -> (tokenlist, [])
   | Lexer.Neq -> (tokenlist, [])
   | Lexer.Less -> (tokenlist, [])
   | Lexer.Leq -> (tokenlist, [])
   | Lexer.Greater -> (tokenlist, [])
   | Lexer.Geq -> (tokenlist, [])
   | Lexer.And -> (tokenlist, [])
   | Lexer.Or -> (tokenlist, [])
   | Lexer.Comma -> (tokenlist, [])
   | _ -> raise error  


(*Actuals_opt  = actuals_list  | epsilon*)
let parseActualsOpt tokenlist = 
  match tokenlist.head with
  | Lexer.Numeral str -> let actuals_list tokenlist_actuals = next tokenlist |> parseActualsList in
                  (tokenlist_actuals, Ast.ActualsOpt(Lexer.Numeral str, actuals_list))
  | Lexer.True -> let actuals_list tokenlist_actuals = next tokenlist |> parseActualsList in
                  (tokenlist_actuals, Ast.ActualsOpt(Lexer.True, actuals_list))
  | Lexer.False -> let actuals_list tokenlist_actuals = next tokenlist |> parseActualsList in
                  (tokenlist_actuals, Ast.ActualsOpt(Lexer.False, actuals_list))
  | Lexer.ID identifier -> let actuals_list tokenlist_actuals = next tokenlist |> parseActualsList in
                  (tokenlist_actuals, Ast.ActualsOpt(Lexer.ID identifier, actuals_list))
  | Lexer.Minus -> let actuals_list tokenlist_actuals = next tokenlist |> parseActualsList in
                  (tokenlist_actuals, Ast.ActualsOpt(Lexer.Minus, actuals_list))
  | Lexer.Not -> let actuals_list tokenlist_actuals = next tokenlist |> parseActualsList in
                  (tokenlist_actuals, Ast.ActualsOpt(Lexer.Not, actuals_list))
  | Lexer.LeftParens -> let actuals_list tokenlist_actuals = next tokenlist |> parseActualsList in
                  (tokenlist_actuals, Ast.ActualsOpt(Lexer.LeftParens, actuals_list))
  | Lexer.RightParens -> expr2
  | _ -> raise error

(*actuals_list = expr actuals_list_prime*)
let parseActualsList tokenlist = 
  match value with
  | patt -> expr
  | Lexer.Numeral str -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  let actuals_list_prime tokenlist_actuals = parseExpr tokenlist_expr in
                  (tokenlist_actuals, Ast.ActualsList(Lexer.Numeral str, expr, actuals_list_prime))
  | Lexer.True -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  let actuals_list_prime tokenlist_actuals = parseExpr tokenlist_expr in
                  (tokenlist_actuals, Ast.ActualsList(Lexer.True, expr, actuals_list_prime))
  | Lexer.False -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  let actuals_list_prime tokenlist_actuals = parseExpr tokenlist_expr in
                  (tokenlist_actuals, Ast.ActualsList(Lexer.False, expr, actuals_list_prime))
  | Lexer.ID identifier -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  let actuals_list_prime tokenlist_actuals = parseExpr tokenlist_expr in
                  (tokenlist_actuals, Ast.ActualsList(Lexer.ID identifier, expr, actuals_list_prime))
  | Lexer.Minus -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  let actuals_list_prime tokenlist_actuals = parseExpr tokenlist_expr in
                  (tokenlist_actuals, Ast.ActualsList(Lexer.Minus, expr, actuals_list_prime))
  | Lexer.Not -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  let actuals_list_prime tokenlist_actuals = parseExpr tokenlist_expr in
                  (tokenlist_actuals, Ast.ActualsList(Lexer.Not, expr, actuals_list_prime))
  | Lexer.LeftParens -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                  let actuals_list_prime tokenlist_actuals = parseExpr tokenlist_expr in
                  (tokenlist_actuals, Ast.ActualsList(Lexer.LeftParens, expr, actuals_list_prime))
  | _ -> raise error

(*Actuals_list_prime = COMMA expr actuals_list_prime | epsilon*)
let rec parseActualsListPrime tokenlist = 
  match tokenlist.head with
  | Lexer.Comma -> let expr tokenlist_expr = next tokenlist |> parseExpr in
                let actuals_list_prime tokenlist_actuals_prime = next tokenlist_expr |> parseExpr in
                (tokenlist_actuals_prime, Ast.ActualsListPrime(Lexer.Comma, expr, actuals_list_prime))
  | Lexer.RightParens -> (tokenlist, [])
  | _ -> raise error

(*Program = decls “EOF”*)
let parseProgram tokenlist = 
    match tokenlist.head with 
    | Lexer.Int -> let decls tokenlist_decls = parseDecls tokenlist in 
               match tokenlist_decls.next with
               | Lexer.EOF -> ([], Program decls)
    | Lexer.Bool -> let decls tokenlist_decls = parseDecls tokenlist in 
               match tokenlist_decls.next with
               | Lexer.EOF -> ([], Program decls)
    | Lexer.Void -> let decls tokenlist_decls = parseDecls tokenlist in 
               match tokenlist_decls.next with
               | Lexer.EOF -> ([], Program decls)
    | Lexer.EOF -> ([], [])
    | _ -> raise error

