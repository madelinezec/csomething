exception Syntax_error of string 
(** Raised when [!Parser] encounters an unrecongnized token. *)

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

let parseTyp tokenlist = 
    match tokenlist.head with 
    | Lexer.Int -> (next tokenlist, Ast.Int) 
    | Lexer.Bool -> (next tokenlist, Ast.Bool)
    | Lexer.Void -> (next tokenlist, Ast.Void)  
    | Lexer.EOF -> (tokenlist, Ast.Epsilon)
    | _-> let err_msg = "Syntax Error" in
          raise (Syntax_error err_msg)

(*decls = typ “id” decls_prime | epsilon *)
let rec parseDecls tokenlist = 
    let typ tokenlist = parseTyp tokenlist in 
        match tokenlist.head with 
        | Lexer.ID identifier -> let decls_prime, tokenlist_decls_prime = next tokenlist |> parseDeclsPrime in 
                                (tokenlist_decls_prime, Ast.Declaration(typ, identifier, decls_prime))
        | Lexer.EOF -> (tokenlist, [])
        | _-> let err_msg = Printf.sprintf "Syntax Error" in
              raise (Syntax_error err_msg)

(* decls_prime = vdecl decls | fdecl decls *)
and parseDeclsPrime tokenlist =
    match tokenlist.head with 
    | Lexer.Semicolon -> let tokenlist_vdecl = next tokenlist in
                let decls tokenlist_decls = parseDecls tokenlist_vdecl in 
                (tokenlist_decls, Ast.DeclsPrime(Lexer.Semicolon, vdecl, decls)) 
    | Lexer.LeftParens -> let fdecl tokenlist_fdecl = next tokenlist |> parseFdecl in 
                let decls tokenlist_decls = parseDecls tokenlist_fdecl in 
                (tokenlist_decls, Ast.DeclsPrime(Lexer.Semicolon, fdecl, decls)) 
    | _-> let err_msg = Printf.sprintf "Syntax Error" in
          raise (Syntax_error err_msg)

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
                                              | _-> let err_msg = Printf.sprintf "Syntax Error" in
                                                      raise (Syntax_error err_msg)
                                | _-> let err_msg = Printf.sprintf "Syntax Error" in
                                      raise (Syntax_error err_msg)
                  | _-> let err_msg = Printf.sprintf "Syntax Error" in
                        raise (Syntax_error err_msg)
    | _-> let err_msg = Printf.sprintf "Syntax Error" in
        raise (Syntax_error err_msg)

(* formals_opt = formal_list | epsilon *)
let parseFormalsOpt tokenlist = 
    match tokenlist.head with 
    | Lexer.Int -> let formal_list tokenlist_formal_list = next tokenlist |> parseFormalList in (tokenlist_formal_list, Ast.FormalsOpt(formal_list))
    | Lexer.Bool -> let formal_list tokenlist_formal_list = next tokenlist |> parseFormalList in (tokenlist_formal_list, Ast.FormalsOpt(formal_list))
    | Lexer.Void -> let formal_list tokenlist_formal_list = next tokenlist |> parseFormalList  in (tokenlist_formal_list, Ast.FormalsOpt(formal_list))
    | Lexer.RightParens -> (tokenlist, [])
    | _-> let err_msg = Printf.sprintf "Syntax Error" in
          raise (Syntax_error err_msg)

(* formal_list = typ “ID” formal_list_prime *)
let parseFormalList tokenlist = 
    match tokenlist.head with 
    | Lexer.Int -> let tokenlist_next = next tokenlist in 
              match tokenlist_next.head with 
               | Lexer.ID identifier -> let tokenlist_typ = next tokenlist_next |> next in 
                         let formal_list_prime tokenlist_formal_list_prime = parseFormalListPrime tokenlist_typ in (tokenlist_formal_list_prime, Ast.FormalList("INT", Lexer.ID identifier, decls_prime))
               | _-> let err_msg = Printf.sprintf "Syntax Error" in
                     raise (Syntax_error err_msg)
    | Lexer.Bool -> let tokenlist_next = next tokenlist in
                match tokenlist_next.head with 
               | Lexer.ID identifier -> let tokenlist_typ = next tokenlist_next |> next in 
                         let formal_list_prime tokenlist_formal_list_prime = parseFormalListPrime tokenlist_typ in (tokenlist_formal_list_prime, Ast.FormalList(Lexer.Bool, Lexer.ID identifier, decls_prime))
               | _-> let err_msg = Printf.sprintf "Syntax Error" in
                      raise (Syntax_error err_msg)
    | Lexer.Void -> let tokenlist_next = next tokenlist in
                match tokenlist_next.head with 
               | Lexer.ID identifier -> let tokenlist_typ = next tokenlist_next |> next in 
                         let formal_list_prime tokenlist_formal_list_prime = parseFormalListPrime tokenlist_typ in (tokenlist_formal_list_prime, Ast.FormalList(Lexer.Void, Lexer.ID identifier, decls_prime))
               | _-> let err_msg = Printf.sprintf "Syntax Error" in
                  raise (Syntax_error err_msg)
    | _-> let err_msg = Printf.sprintf "Syntax Error" in
          raise (Syntax_error err_msg)

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
    | Lexer.Semicolon -> (next tokenlist, [])

(*stmt_list = stmt stmt_list | epsilon*)
let rec parseStmtList tokenlist =
    match tokenlist.head with 
    | Lexer.Semicolon -> let stmt tokenlist_stmt = parseStmt tokenlist in 
                        let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                        (tokenlist_stmt_list, Ast.StmtList(Lexer.Semicolon, stmt, stmt_list))
    | Lexer.LeftParens -> let stmt tokenlist_stmt = parseStmt tokenlist in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList(Lexer.LeftParens, stmt, stmt_list))
    | Lexer.Return -> let stmt tokenlist_stmt = parseStmt tokenlist in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.RETURN(stmt, stmt_list))
    | Lexer.LeftBrace -> let stmt tokenlist_stmt = parseStmt tokenlist in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList(Lexer.LeftBrace, stmt, stmt_list))
    | Lexer.RightBrace -> (tokenlist.next, [])
    | Lexer.If -> let stmt tokenlist_stmt = parseStmt tokenlist in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.IF(stmt, stmt_list))
    | Lexer.For -> let stmt tokenlist_stmt = parseStmt tokenlist in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.FOR( stmt, stmt_list))
    | Lexer.While -> let stmt tokenlist_stmt = parseStmt tokenlist in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.WHILE( stmt, stmt_list))
    | Lexer.Numeral str -> let stmt tokenlist_stmt = parseStmt tokenlist in  
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.LITERAL(stmt, stmt_list))
    | Lexer.Minus -> let stmt tokenlist_stmt = parseStmt tokenlist in  
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.MinuMINUS(stmt, stmt_list))
    | Lexer.Not -> let stmt tokenlist_stmt = parseStmt tokenlist in  
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList(Lexer.Not, stmt, stmt_list))
    | Lexer.ID identifier -> let stmt tokenlist_stmt = parseStmt tokenlist in  
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList(Lexer.ID identifier, stmt, stmt_list))
    | Lexer.True -> let stmt tokenlist_stmt = parseStmt tokenlist in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList(Lexer.True, stmt, stmt_list))
    | Lexer.False -> let stmt tokenlist_stmt = parseStmt tokenlist in  
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.If(stmt, stmt_list))
    | _-> let err_msg = Printf.sprintf "Syntax Error" in
          raise (Syntax_error err_msg)


(* assignment -> ID assignmentType
assignment -> expr
assignmentType -> typ ASSIGN expr 
assignmentType -> ASSSIGN expr*)
let parseAssignment tokenlist = 
  match tokenlist.head with
  |Lexer.ID identifier -> let assignmentType tokenlist_assignment = next tokenlist |> parseAssignmentType in 
            (tokenlist_assignment, Ast.AID(assignmentType))
  |Lexer.LeftParens -> let expr tokenlist_expr = parseExpr tokenlist in 
              (tokenlist_expr, Ast.AExpr(expr))
  |Lexer.Numeral str -> let expr tokenlist_expr = parseExpr tokenlist in 
              (tokenlist_expr, Ast.AExpr(expr))
  |Lexer.True -> let expr tokenlist_expr = parseExpr tokenlist in 
              (tokenlist_expr, Ast.AExpr(expr))
  |Lexer.False -> let expr tokenlist_expr = parseExpr tokenlist in 
              (tokenlist_expr, Ast.AExpr(expr))
  | _-> let err_msg = Printf.sprintf "Syntax Error" in
          raise (Syntax_error err_msg) 

(* assignmentType -> typ ASSIGN expr 
assignmentType -> ASSSIGN expr*)
let parseAssignmentType tokenlist = 
  match tokenlist.head with
  |Lexer.Equality -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                (tokenlist_expr, Ast.ASSIGN(expr))
  |Lexer.Numeral str -> let typ tokenlist_typ = parseTyp tokenlist in 
            match tokenlist_typ.lookathead with
            | Lexer.Equality -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                          (tokenlist_expr, Ast.ASSIGN(typ, expr))
            | _-> let err_msg = Printf.sprintf "Syntax Error" in
                  raise (Syntax_error err_msg)
  |Lexer.Bool -> let typ tokenlist_typ = parseTyp tokenlist in 
            match tokenlist_typ.lookathead with
            | Lexer.Equality -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                          (tokenlist_expr, Ast.ASSIGN(typ, expr))
            | _-> let err_msg = Printf.sprintf "Syntax Error" in
                  raise (Syntax_error err_msg)
  |Lexer.Void -> let typ tokenlist_typ = parseTyp tokenlist in 
            match tokenlist_typ.lookathead with
            | Lexer.Equality -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                          (tokenlist_expr, Ast.ASSIGN(typ, expr))
            | _-> let err_msg = Printf.sprintf "Syntax Error" in
                  raise (Syntax_error err_msg)
  | _-> let err_msg = Printf.sprintf "Syntax Error" in
        raise (Syntax_error err_msg)

(*stmt -> assignment SEMI 
|  RETURN stmt_opt 
|  LBRACE stmt_list RBRACE 
|  IF LPAREN assignment RPAREN stmt 
|  FOR LPAREN assignment SEMI assignment SEMI assignment RPAREN stmt  
|  WHILE LPAREN assignment RPAREN stmt
*)

let parseStmt tokenlist = 
   match tokenlist.head with 
   | Lexer.ID identifier -> let assignment tokenlist_assignment = parseAssignment tokenlist in
              (tokenlist_assignment, Ast.Assign(assignment))
   |  Lexer.LeftParens -> let assignment tokenlist_assignment = parseAssignment tokenlist in
              (tokenlist_assignment, Ast.Assign(assignment))
   | Lexer.Numeral str -> let assignment tokenlist_assignment = parseAssignment tokenlist in
              (tokenlist_assignment, Ast.Assign(assignment))
   | Lexer.Bool -> let assignment tokenlist_assignment = parseAssignment tokenlist in
              (tokenlist_assignment, Ast.Assign(assignment))
   | Lexer.Bool -> let assignment tokenlist_assignment = parseAssignment tokenlist in
              (tokenlist_assignment, Ast.Assign(assignment))

   | Lexer.Return -> let stmt_prime tokenlist_stmt_opt = next tokenlist |> parseStmtOpt in 
                 (tokenlist_stmt_opt, Ast.Return(stmt_prime))

   | Lexer.If -> let tokenlist_lparen = next tokenlist |> next in 
             match tokenlist_lparen with 
             | Lexer.LeftParens -> let expr tokenlist_assignment = next tokenlist_lparen |> parseAssignment in 
                 match next tokenlist_assignment with 
                 | Lexer.RightParens -> let stmt tokenlist_stmt = next tokenlist_assignment |> parseStmt in 
                     (tokenlist_stmt_prime, Ast.If(assignment, stmt)) 
                 | _-> let err_msg = Printf.sprintf "Syntax Error" in
                       raise (Syntax_error err_msg)
             | _-> let err_msg = Printf.sprintf "Syntax Error" in
                    raise (Syntax_error err_msg)
   | Lexer.For -> let nexthead = next tokenlist in
            match nexthead with
            | Lexer.LeftParens -> let expr tokenlist_assignment = parseAssignment nexthead in 
                          match tokenlist_assignment.lookathead with
                          | Lexer.Semicolon -> let expr2 tokenlist_assignment2 = next tokenlist_assignment |> parseAssignment in
                                      match tokenlist_assignment2.lookathead with 
                                      | Lexer.Semicolon -> let expr3 tokenlist_assignment3 = next tokenlist_assignment2 |> parseAssignment in 
                                                  match  tokenlist_assignment3.lookathead with
                                                  | Lexer.RightParens -> let stmt tokenlist_stmt = next tokenlist_assignment3 |> parseStmt in
                                                  (tokenlist_stmt, Ast.For(expr, expr2, expr3, stmt))
                                                  | _-> let err_msg = Printf.sprintf "Syntax Error" in
                                                        raise (Syntax_error err_msg)
                                        | _-> let err_msg = Printf.sprintf "Syntax Error" in
                                              raise (Syntax_error err_msg)
                          | _-> let err_msg = Printf.sprintf "Syntax Error" in
                                  raise (Syntax_error err_msg)
   | Lexer.While -> let tokenlist_lparen = next tokenlist in 
                match tokenlist_lparen.lookathead with
                | Lexer.LeftParens -> let assignment tokenlist_assignment = next tokenlist_lparen |> parseAssignment in
                              match tokenlist_assignment.lookathead with
                              | Lexer.RightParens -> let stmt tokenlist_stmt = next tokenlist_assignment |> parseStmt in
                                            (next tokenlist_stmt, Ast.While(assignment, stmt))
                              | _-> let err_msg = Printf.sprintf "Syntax Error" in
                                    raise (Syntax_error err_msg)
                | _-> let err_msg = Printf.sprintf "Syntax Error" in
                    raise (Syntax_error err_msg)
   | Lexer.LeftBrace -> let tokenlist_leftbrace = next tokenlist in 
                        let expr tokenlist_expr = parseStmtList tokenlist_leftbrace in
                        match tokenlist_expr.lookathead with
                        | Lexer.RightBrace -> (next tokenlist_expr, Ast(expr))
                        | _-> let err_msg = Printf.sprintf "Syntax Error" in
                              raise (Syntax_error err_msg)
   | _-> let err_msg = Printf.sprintf "Syntax Error" in
                    raise (Syntax_error err_msg)
(*stmt_prime ->SEMI| expr SEMI*)
let parseStmtOpt tokenlist = 
     match tokenlist.head with 
     | Lexer.Semicolon -> (next tokenlist, Ast.Semi)
     | Lexer.LeftParens -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                      match tokenlist_expr.head with 
                      |"SEMI" -> (next tokenlist_expr, Ast.StmtOpt(expr)) 
     | Lexer.Numeral str -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                      match tokenlist_expr.head with 
                      |"SEMI" -> (next tokenlist_expr, Ast.StmtOpt(expr)) 
     | Lexer.Id identifier -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                      match tokenlist_expr.head with 
                      |"SEMI" -> (next tokenlist_expr, Ast.StmtOpt(expr)) 
     | Lexer.Bool -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                      match tokenlist_expr.head with 
                      |"SEMI" -> (next tokenlist_expr, Ast.StmtOpt(expr)) 
     | Lexer.Bool -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                      match tokenlist_expr.head with 
                      |"SEMI" -> (next tokenlist_expr, Ast.StmtOpt(expr)) 
     | _-> let err_msg = Printf.sprintf "Syntax Error" in
            raise (Syntax_error err_msg) 



(* expr = T EPRIME
*)
let parseExpr tokenlist =
    match tokenlist.head with 
    | Lexer.LeftParens -> let t_expr tokenlist_t = next tokenlist |> parseExpr in 
                  let e_expr tokenlist_e = parseEPrime tokenlist_t in
                  (tokenlist_e, Ast.Expression(t_expr, e_expr))
    | Lexer.Numeral str -> let t_expr tokenlist_t = next tokenlist |> parseExpr in 
                  let e_expr tokenlist_e = parseEPrime tokenlist_t in
                  (tokenlist_e, Ast.Expression(t_expr, e_expr))
    | Lexer.True -> let t_expr tokenlist_t = next tokenlist |> parseExpr in 
                  let e_expr tokenlist_e = parseEPrime tokenlist_t in
                  (tokenlist_e, Ast.Expression(t_expr, e_expr))
    | Lexer.False -> let t_expr tokenlist_t = next tokenlist |> parseExpr in 
                  let e_expr tokenlist_e = parseEPrime tokenlist_t in
                  (tokenlist_e, Ast.Expression(t_expr, e_expr))
    | Lexer.ID identifier -> let t_expr tokenlist_t = next tokenlist |> parseExpr in 
                  let e_expr tokenlist_e = parseEPrime tokenlist_t in
                  (tokenlist_e, Ast.Expression(t_expr, e_expr))
    | _-> let err_msg = Printf.sprintf "Syntax Error" in
          raise (Syntax_error err_msg)

(*expr_prime = E* -> + T E* 
E* -> - T E* 
E* -> ε
*)
let parseEPrime tokenlist =
  match tokenlist with
   | Lexer.Plus -> let expr_t tokenlist_t = next tokenlist |> parseT in
                let expr_eprime tokenlist_e = parseEPrime tokenlist_t in 
                (tokenlist_e, Ast.Add(expr_t, expr_eprime))
   | Lexer.Minus -> let expr_t tokenlist_t = next tokenlist |> parseT in
                let expr_eprime tokenlist_e = parseEPrime tokenlist_t in 
                (tokenlist_e, Ast.Minus(expr_t, expr_eprime))
   | Lexer.Semicolon -> (tokenlist, [])
   | Lexer.RightParens -> (tokenlist, [])
   | _-> let err_msg = Printf.sprintf "Syntax Error" in
         raise (Syntax_error err_msg)

(* T -> F T**)
let parseT tokenlist = 
  match tokenlist.lookathead with 
  | Lexer.LeftParens -> let expr_f tokenlist_f = parseF tokenlist in 
                let expr_tprime tokenlist_tprime = parseTprime tokenlist_f in 
                (tokenlist_tprime, Ast.F(expr_f, expr_tprime))
  | Lexer.Numeral -> let expr_f tokenlist_f = parseF tokenlist in 
                let expr_tprime tokenlist_tprime = parseTprime tokenlist_f in 
                (tokenlist_tprime, Ast.Literal(expr_f, expr_tprime))
  | Lexer.Bool -> let expr_f tokenlist_f = parseF tokenlist in 
                let expr_tprime tokenlist_tprime = parseTprime tokenlist_f in 
                (tokenlist_tprime, Ast.F(expr_f, expr_tprime))
  | Lexer.Bool -> let expr_f tokenlist_f = parseF tokenlist in 
                let expr_tprime tokenlist_tprime = parseTprime tokenlist_f in 
                (tokenlist_tprime, Ast.F(expr_f, expr_tprime))
  | Lexer.ID identifier -> let expr_f tokenlist_f = parseF tokenlist in 
                let expr_tprime tokenlist_tprime = parseTprime tokenlist_f in 
                (tokenlist_tprime, Ast.F(expr_f, expr_tprime))
  | _-> let err_msg = Printf.sprintf "Syntax Error" in
        raise (Syntax_error err_msg)
(*T* -> * F T* 
T* -> / F T* 
T* -> ε *)
let parseTprime tokenlist = 
  match  tokenlist.lookathead with
  | "TIMES" -> let expr_f tokenlist_f = next tokenlist |> parseF in 
                let expr_tprime tokenlist_tprime = parseTPrime tokenlist_f in 
                (tokenlist_tprime, Ast.Times(expr_f, expr_tprime))
  | Lexer.Divide -> let expr_f tokenlist_f = next tokenlist |> parseF in 
                let expr_tprime tokenlist_tprime = parseTPrime tokenlist_f in 
                (tokenlist_tprime, Ast.Divide(expr_f, expr_tprime))
  | Lexer.Plus -> (tokenlist, [])
  | Lexer.Minus -> (tokenlist, [])
  | Lexer.Semicolon -> (tokenlist, [])
  | Lexer.RightParens -> (tokenlist, [])
  | _-> let err_msg = Printf.sprintf "Syntax Error" in
        raise (Syntax_error err_msg)

(* F -> LPAREN E RPAREN 
F -> int_literal 
F -> TRUE 
F -> FALSE
F -> ID*)
let parseF tokenlist = 
  match tokenlist.lookathead with
  | Lexer.LeftParens -> let expr tokenlist_expr = next tokenlist |> parseE in 
                match next tokenlist_expr with 
                | "RPAREN" -> (next tokenlist_expr, Ast.F(expr))
  | Lexer.Numeral str -> (next tokenlist, Ast.FLiteral)
  | Lexer.Bool -> (next tokenlist, Ast.BoolLit)
  | Lexer.Bool -> (next tokenlist, Ast.FBool)
  | Lexer.ID identifier -> (next tokenlist, Ast.BoolLit)
  | _-> let err_msg = Printf.sprintf "Syntax Error" in
        raise (Syntax_error err_msg)



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
    | _-> let err_msg = Printf.sprintf "Syntax Error" in
          raise (Syntax_error err_msg)

