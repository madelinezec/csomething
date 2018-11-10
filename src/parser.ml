open Lexer
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
    | Lexer.INT -> (tokenlist.next, Int) 
    | Lexer.BOOL -> (tokenlist.next, Bool)
    | Lexer.VOID -> (tokenlist.next, Void)  
    | Lexer.EOF -> (tokenlist, [])

(*decls = typ “id” decls_prime | epsilon *)
let parseDecls tokenlist = 
    let typ tokenlist_typ = parseTyp(tokenlist) in 
    match tokenlist_typ.lookathead with 
    | "ID" -> let decls_prime tokenlist_decls_prime = next tokenlist_typ |> parseDeclsPrime in 
                  (tokenlist_decls_prime, Ast.Decls(typ, identifier, decls_prime))
    | "EOF" -> (tokenlist, [])
    | _-> raise error

(* decls_prime = vdecl decls | fdecl decls *)
let parseDeclsPrime tokenlist =
    match tokenlist.head with 
    | "SEMI" -> let tokenlist_vdecl = next tokenlist in
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
                                              | "RBRACE" -> (next tokenlist_stmt_list, Ast.Fdecl("LPAREN", formals_opt, "RPAREN", "LBRACE", vdecl_list, stmt_list, "RBRACE"))
                                              | _-> raise error
                                | _-> raise error
                  | _-> raise error
    | _-> raise error

(* formals_opt = formal_list | epsilon *)
let parseFormalsOpt tokenlist = 
    match tokenlist.head with 
    | "INT" -> let formal_list tokenlist_formal_list = next tokenlist |> parseFormalList in (tokenlist_formal_list, Ast.FormalsOpt("INT", formal_list))
    | "BOOL" -> let formal_list tokenlist_formal_list = next tokenlist |> parseFormalList in (tokenlist_formal_list, Ast.FormalsOpt("BOOL", formal_list))
    | "VOID" -> let formal_list tokenlist_formal_list = next tokenlist |> parseFormalList  in (tokenlist_formal_list, Ast.FormalsOpt("VOID", formal_list))
    | "RPAREN" -> (tokenlist, [])
    | _-> raise error

(* formal_list = typ “ID” formal_list_prime *)
let parseFormalList tokenlist = 
    match tokenlist.head with 
    | "INT" -> let tokenlist_next = next tokenlist in 
              match tokenlist_next.head with 
               | "ID" -> let tokenlist_typ = next tokenlist_next |> next in 
                         let formal_list_prime tokenlist_formal_list_prime = parseFormalListPrime tokenlist_typ in (tokenlist_formal_list_prime, Ast.FormalList("INT", "ID", decls_prime))
               | _ -> raise error
    | "BOOL" -> let tokenlist_next = next tokenlist in
                match tokenlist_next.head with 
               | "ID" -> let tokenlist_typ = next tokenlist_next |> next in 
                         let formal_list_prime tokenlist_formal_list_prime = parseFormalListPrime tokenlist_typ in (tokenlist_formal_list_prime, Ast.FormalList("BOOL", "ID", decls_prime))
               | _ -> raise error
    | "VOID" -> let tokenlist_next = next tokenlist in
                match tokenlist_next.head with 
               | "ID" -> let tokenlist_typ = next tokenlist_next |> next in 
                         let formal_list_prime tokenlist_formal_list_prime = parseFormalListPrime tokenlist_typ in (tokenlist_formal_list_prime, Ast.FormalList("VOID", "ID", decls_prime))
               | _ -> raise error
    | _-> raise error

(* vdecl_list = vdecl vdecl_list | “epsilon”*)
let rec parseVdeclList tokenlist = 
    match tokenlist.head with 
    | "SEMI" -> let tokenlist_vdecl = next tokenlist in
                let vdecl_list tokenlist_vdecl_list = parseVdeclList tokenlist_vdecl in 
                (tokenlist_vdecl_list, Ast.VdeclList("SEMI", vdecl list))
    | "LPAREN" -> (next tokenlist, [])
    | "RETURN" -> (next tokenlist, [])
    | "LBRACE" -> (next tokenlist, [])
    | "IF" -> (next tokenlist, [])
    | "FOR" -> (next tokenlist, [])
    | "WHILE" -> (next tokenlist, [])
    | "LITERAL" -> (next tokenlist, [])
    | "MINUS" -> (next tokenlist, [])
    | "NOT" -> (next tokenlist, [])
    | "ID" -> (next tokenlist, Ast.Lparen)

(* vdecl = “SEMI” *)
(* later we can remove this function*)
let parseVdecl tokenlist = 
    match tokenlist.head with 
    | "SEMI" -> (next tok[][])

(*stmt_list = stmt stmt_list | epsilon*)
let rec parseStmtList tokenlist =
    match tokenlist.head with 
    | "RBRACE" -> (tokenlist.next, [])
    | "SEMI" -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList("SEMI", stmt, stmt_list))
    | "LPAREN" -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList("LPAREN", stmt, stmt_list))
    | "RETURN" -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.RETURN(stmt, stmt_list))
    | "LBRACE" -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList("LBRACE", stmt, stmt_list))
    | "IF" -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.IF(stmt, stmt_list))
    | "FOR" -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.FOR( stmt, stmt_list))
    | "WHILE" -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.WHILE( stmt, stmt_list))
    | "LITERAL" -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.LITERAL(stmt, stmt_list))
    | "ID" -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList("ID", stmt, stmt_list))
    | "TRUE" -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList("TRUE", stmt, stmt_list))
    | "FALSE" -> let stmt tokenlist_stmt = next tokenlist |> parseStmt in 
                let stmt_list tokenlist_stmt_list = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList("IF", stmt, stmt_list))
   | _-> raise error


(* assignment -> ID assignmentType
assignment -> expr
assignmentType -> typ ASSIGN expr 
assignmentType -> ASSSIGN expr*)
let parseAssignment tokenlist = 
  match tokenlist.lookathead with
  |"ID" -> let assignmentType tokenlist_assignment = next tokenlist |> parseAssignmentType in 
            (tokenlist_assignment, Ast.AID(assignmentType))
  |"LPAREN" -> let expr tokenlist_expr = parseExpr tokenlist in 
              (tokenlist_expr, Ast.AExpr(expr))
  |"LITERAL" -> let expr tokenlist_expr = parseExpr tokenlist in 
              (tokenlist_expr, Ast.AExpr(expr))
  |"TRUE" -> let expr tokenlist_expr = parseExpr tokenlist in 
              (tokenlist_expr, Ast.AExpr(expr))
  |"FALSE" -> let expr tokenlist_expr = parseExpr tokenlist in 
              (tokenlist_expr, Ast.AExpr(expr))
  | _ -> raise error  

(* assignmentType -> typ ASSIGN expr 
assignmentType -> ASSSIGN expr*)
let parseAssignmentType tokenlist = 
  match tokenlist.head with
  |"ASSIGN" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                (tokenlist_expr, Ast.ASSIGN(expr))
  |"INT" -> let typ tokenlist_typ = parseTyp tokenlist in 
            match tokenlist_typ.lookathead with
            | "ASSIGN" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                          (tokenlist_expr, Ast.ASSIGN(typ, expr))
            | _ -> raise error
  |"BOOL" -> let typ tokenlist_typ = parseTyp tokenlist in 
            match tokenlist_typ.lookathead with
            | "ASSIGN" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                          (tokenlist_expr, Ast.ASSIGN(typ, expr))
            | _ -> raise error
  |"VOID" -> let typ tokenlist_typ = parseTyp tokenlist in 
            match tokenlist_typ.lookathead with
            | "ASSIGN" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                          (tokenlist_expr, Ast.ASSIGN(typ, expr))
            | _ -> raise error
  | _ -> raise error

(*stmt -> assignment SEMI 
|  RETURN stmt_opt 
|  LBRACE stmt_list RBRACE 
|  IF LPAREN assignment RPAREN stmt 
|  FOR LPAREN assignment SEMI assignment SEMI assignment RPAREN stmt  
|  WHILE LPAREN assignment RPAREN stmt
*)

let parseStmt tokenlist = 
   match tokenlist.head with 
   | "ID" -> let assignment tokenlist_assignment = parseAssignment tokenlist in
              (tokenlist_assignment, Ast.Assign(assignment))
   | "LPAREN" -> let assignment tokenlist_assignment = parseAssignment tokenlist in
              (tokenlist_assignment, Ast.Assign(assignment))
   | "LITERAL" -> let assignment tokenlist_assignment = parseAssignment tokenlist in
              (tokenlist_assignment, Ast.Assign(assignment))
   | "TRUE" -> let assignment tokenlist_assignment = parseAssignment tokenlist in
              (tokenlist_assignment, Ast.Assign(assignment))
   | "False" -> let assignment tokenlist_assignment = parseAssignment tokenlist in
              (tokenlist_assignment, Ast.Assign(assignment))

   | "RETURN" -> let stmt_prime tokenlist_stmt_opt = next tokenlist |> parseStmtOpt in 
                 (tokenlist_stmt_prime, Ast.Return(stmt_prime))
   | "LBRACE" -> let stmt_list tokenlist_stmt_list = next tokenlist |> parseStmtList in 
                 match tokenlist_stmt_list.head with 
                 | "RBRACE" -> (next tokenlist_stmt_list, AST.StmtList(stmt_list))
                 | _ -> raise error 
   | "IF" -> let tokenlist_lparen = next tokenlist |> next in 
             match tokenlist_lparen with 
             | "LPAREN" -> let expr tokenlist_assignment = next tokenlist_lparen |> parseAssignment in 
                 match next tokenlist_assignment with 
                 | "RPAREN" -> let stmt tokenlist_stmt = next tokenlist_assignment |> parseStmt in 
                     (tokenlist_stmt_prime, Ast.If(assignment, stmt)) 
                 | _-> raise error
             | _-> raise error
   | "FOR" -> let nexthead = next tokenlist in
            match nexthead with
            | "LPAREN" -> let expr tokenlist_assignment = parseAssignment nexthead in 
                          match tokenlist_assignment.lookathead with
                          | "SEMI" -> let expr2 tokenlist_assignment2 = next tokenlist_assignment |> parseAssignment in
                                      match tokenlist_assignment2.lookathead with 
                                      | "SEMI" -> let expr3 tokenlist_assignment3 = next tokenlist_assignment2 |> parseAssignment in 
                                                  match  tokenlist_assignment3.lookathead with
                                                  | "RPAREN" -> let stmt tokenlist_stmt = next tokenlist_assignment3 |> parseStmt in
                                                  (tokenlist_stmt, Ast.For(expr, expr2, expr3, stmt))
                                                  | _ -> raise error
                                        | _-> raise error
                          | _-> raise error
   | "WHILE" -> let tokenlist_lparen = next tokenlist in 
                match tokenlist_lparen.lookathead with
                | "LPAREN" -> let assignment tokenlist_assignment = next tokenlist_lparen |> parseAssignment in
                              match tokenlist_assignment.lookathead with
                              | "RPAREN" -> let stmt tokenlist_stmt = next tokenlist_assignment |> parseStmt in
                                            (next tokenlist_stmt, Ast.While(assignment, stmt))
                              | _ -> raise error
                | _ -> raise error
   | _-> raise error
(*stmt_prime ->SEMI| expr SEMI*)
let parseStmtOpt tokenlist = 
     match tokenlist.head with 
     | "SEMI" -> (next tokenlist, Ast.Semi)
     | "LPAREN" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                      match tokenlist_expr.head with 
                      |"SEMI" -> (next tokenlist_expr, Ast.StmtOpt(expr)) 
     | "LITERAL" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                      match tokenlist_expr.head with 
                      |"SEMI" -> (next tokenlist_expr, Ast.StmtOpt(expr)) 
     | "ID" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                      match tokenlist_expr.head with 
                      |"SEMI" -> (next tokenlist_expr, Ast.StmtOpt(expr)) 
     | "TRUE" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                      match tokenlist_expr.head with 
                      |"SEMI" -> (next tokenlist_expr, Ast.StmtOpt(expr)) 
     | "FALSE" -> let expr tokenlist_expr = next tokenlist |> parseExpr in 
                      match tokenlist_expr.head with 
                      |"SEMI" -> (next tokenlist_expr, Ast.StmtOpt(expr)) 
     | _ -> raise error  


(* expr = T EPRIME
*)
let parseExpr tokenlist =
    match tokenlist.head with 
    | "LPAREN" -> let t_expr tokenlist_t = next tokenlist |> parseExpr in 
                  let e_expr tokenlist_e = parseEPrime tokenlist_t in
                  (tokenlist_e, Ast.Expression(t_expr, e_expr))
    | "LITERAL" -> let t_expr tokenlist_t = next tokenlist |> parseExpr in 
                  let e_expr tokenlist_e = parseEPrime tokenlist_t in
                  (tokenlist_e, Ast.Expression(t_expr, e_expr))
    | "TRUE" -> let t_expr tokenlist_t = next tokenlist |> parseExpr in 
                  let e_expr tokenlist_e = parseEPrime tokenlist_t in
                  (tokenlist_e, Ast.Expression(t_expr, e_expr))
    | "FALSE" -> let t_expr tokenlist_t = next tokenlist |> parseExpr in 
                  let e_expr tokenlist_e = parseEPrime tokenlist_t in
                  (tokenlist_e, Ast.Expression(t_expr, e_expr))
    | "ID" -> let t_expr tokenlist_t = next tokenlist |> parseExpr in 
                  let e_expr tokenlist_e = parseEPrime tokenlist_t in
                  (tokenlist_e, Ast.Expression(t_expr, e_expr))

(*expr_prime = E* -> + T E* 
E* -> - T E* 
E* -> ε
*)
let parseEPrime tokenlist =
  match tokenlist with
   | "PLUS" -> let expr_t tokenlist_t = next tokenlist |> parseT in
                let expr_eprime tokenlist_e = parseEPrime tokenlist_t in 
                (tokenlist_e, Ast.Add(expr_t, expr_eprime))
   | "MINUS" -> let expr_t tokenlist_t = next tokenlist |> parseT in
                let expr_eprime tokenlist_e = parseEPrime tokenlist_t in 
                (tokenlist_e, Ast.Minus(expr_t, expr_eprime))
   | "SEMI" -> (tokenlist, [])
   | "RPAREN" -> (tokenlist, [])
   | _ -> raise error  

(* T -> F T**)
let parseT tokenlist = 
  match tokenlist.lookathead with 
  | "LPAREN" -> let expr_f tokenlist_f = parseF tokenlist in 
                let expr_tprime tokenlist_tprime = parseTprime tokenlist_f in 
                (tokenlist_tprime, Ast.F(expr_f, expr_tprime))
  | "LITERAL" -> let expr_f tokenlist_f = parseF tokenlist in 
                let expr_tprime tokenlist_tprime = parseTprime tokenlist_f in 
                (tokenlist_tprime, Ast.Literal(expr_f, expr_tprime))
  | "TRUE" -> let expr_f tokenlist_f = parseF tokenlist in 
                let expr_tprime tokenlist_tprime = parseTprime tokenlist_f in 
                (tokenlist_tprime, Ast.F(expr_f, expr_tprime))
  | "FALSE" -> let expr_f tokenlist_f = parseF tokenlist in 
                let expr_tprime tokenlist_tprime = parseTprime tokenlist_f in 
                (tokenlist_tprime, Ast.F(expr_f, expr_tprime))
  | "ID" -> let expr_f tokenlist_f = parseF tokenlist in 
                let expr_tprime tokenlist_tprime = parseTprime tokenlist_f in 
                (tokenlist_tprime, Ast.F(expr_f, expr_tprime))
  | _-> raise error
(*T* -> * F T* 
T* -> / F T* 
T* -> ε *)
let parseTprime tokenlist = 
  match  tokenlist.lookathead with
  | "TIMES" -> let expr_f tokenlist_f = next tokenlist |> parseF in 
                let expr_tprime tokenlist_tprime = parseTPrime tokenlist_f in 
                (tokenlist_tprime, Ast.Times(expr_f, expr_tprime))
  | "DIVIDE" -> let expr_f tokenlist_f = next tokenlist |> parseF in 
                let expr_tprime tokenlist_tprime = parseTPrime tokenlist_f in 
                (tokenlist_tprime, Ast.Divide(expr_f, expr_tprime))
  | "PLUS" -> (tokenlist, [])
  | "MINUS" -> (tokenlist, [])
  | "SEMI" -> (tokenlist, [])
  | "RPAREN" -> (tokenlist, [])
  | _ -> raise error  

(* F -> LPAREN E RPAREN 
F -> int_literal 
F -> TRUE 
F -> FALSE
F -> ID*)
let parseF tokenlist = 
  match tokenlist.lookathead with
  | "LPAREN" -> let expr tokenlist_expr = next tokenlist |> parseE in 
                match next tokenlist_expr with 
                | "RPAREN" -> (next tokenlist_expr, Ast.F(expr))
  | "LITERAL" -> (next tokenlist, Ast.FLiteral)
  | "TRUE" -> (next tokenlist, Ast.BoolLit)
  | "FALSE" -> (next tokenlist, Ast.FBool)
  | "ID" -> (next tokenlist, Ast.BoolLit)
  | _ -> raise error


(*Program = decls “EOF”*)
let parseProgram tokenlist = 
    match tokenlist.head with 
    | "Int" -> let decls tokenlist_decls = parseDecls tokenlist in 
               match tokenlist_decls.next with
               | "EOF" -> ([], Program decls)
    | "BOOL" -> let decls tokenlist_decls = parseDecls tokenlist in 
               match tokenlist_decls.next with
               | "EOF" -> ([], Program decls)
    | "VOID" -> let decls tokenlist_decls = parseDecls tokenlist in 
               match tokenlist_decls.next with
               | "EOF" -> ([], Program decls)
    | "EOF" -> ([], [])
    | _ -> raise error

