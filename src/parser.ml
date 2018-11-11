exception Syntax_error of string 
(** Raised when [!Parser] encounters an unrecongnized token. *)

type token_list = 
  {head : Lexer.token; (** head token. *)
   lexbuf : Lexer.token list}  (** lexer buffer. *)
(** Represents a parser buffer used during parsing of various productions. *)
  [@@deriving show]


let next tokenlist =
    let {head = _; lexbuf = buf} = tokenlist in
    {head = List.hd buf; lexbuf = List.tl buf}
(** Retrieves a new parser buffer with the next lookahead token. *)

let default_tokenlist filename = next {head = Lexer.EOF; lexbuf = Lexer.tokenize_file filename}
(* Create a default [parse_buffer] with the given string [s]. *)

let parseTyp tokenlist = 
    match tokenlist.head with 
    | Lexer.Int -> (next tokenlist, Ast.Int) 
    | Lexer.Bool -> (next tokenlist, Ast.Bool)
    | Lexer.Void -> (next tokenlist, Ast.Void)  
    | Lexer.EOF -> (tokenlist, Ast.Epsilon)
    | _-> let err_msg =  __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
          raise (Syntax_error err_msg)

(* formal_list = typ “ID” formal_list_prime *)
let rec parseFormalList tokenlist = 
    match tokenlist.head with 
    | Lexer.Int -> let (tokenlist_typ, typ) = parseTyp tokenlist in
               begin
               match tokenlist_typ.head with 
               | Lexer.ID identifier -> let (tokenlist_formal_list_prime, formal_list_prime) = parseFormalListPrime tokenlist_typ in 
                                        (tokenlist_formal_list_prime, Ast.FormalList(typ, identifier, formal_list_prime))
               | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                      raise (Syntax_error err_msg);
               end
    | Lexer.Bool -> let (tokenlist_typ, typ) = parseTyp tokenlist in
               begin
               match tokenlist_typ.head with 
               | Lexer.ID identifier -> let (tokenlist_formal_list_prime, formal_list_prime) = parseFormalListPrime tokenlist_typ in 
                                        (tokenlist_formal_list_prime, Ast.FormalList(typ, identifier, formal_list_prime))
               | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                     raise (Syntax_error err_msg)
                end
    | Lexer.Void -> let (tokenlist_typ, typ) = parseTyp tokenlist in
              begin
               match tokenlist_typ.head with 
               | Lexer.ID identifier -> let (tokenlist_formal_list_prime, formal_list_prime) = parseFormalListPrime tokenlist_typ in 
                                        (tokenlist_formal_list_prime, Ast.FormalList(typ, identifier, formal_list_prime))
               | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                     raise (Syntax_error err_msg)
              end
    | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
          raise (Syntax_error err_msg)

(* Formal_list_prime = “COMMA” formal_list | epsilon*)
and parseFormalListPrime tokenlist = 
    match tokenlist.head with
    | Lexer.Comma -> let (tokenlist_formal_list, formal_list) = next tokenlist |> parseFormalList in 
                        (tokenlist_formal_list , Ast.FormalListPrime(formal_list))
    | Lexer.RightParens -> (tokenlist, Ast.Fempty)
    | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
           raise (Syntax_error err_msg)
(* formals_opt = formal_list | epsilon *)
let parseFormalsOpt tokenlist = 
    match tokenlist.head with 
    | Lexer.Int -> let (tokenlist_formal_list, formal_list) = next tokenlist |> parseFormalList in (tokenlist_formal_list, Ast.FormalsOpt(formal_list))
    | Lexer.Bool -> let (tokenlist_formal_list, formal_list) = next tokenlist |> parseFormalList in (tokenlist_formal_list, Ast.FormalsOpt(formal_list))
    | Lexer.Void -> let (tokenlist_formal_list, formal_list) = next tokenlist |> parseFormalList  in (tokenlist_formal_list, Ast.FormalsOpt(formal_list))
    | Lexer.RightParens -> (tokenlist, Ast.FormalsOptEmpty)
    | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
          raise (Syntax_error err_msg)

(* vdecl = “SEMI” *)
(* later we can remove this function*)
let parseVdecl tokenlist = 
    match tokenlist.head with 
    | Lexer.Semicolon -> (next tokenlist, [])
    | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
        raise (Syntax_error err_msg)

(* vdecl_list = vdecl vdecl_list | “epsilon”*)
let rec parseVdeclList tokenlist = 
    match tokenlist.head with 
    | Lexer.Semicolon -> let tokenlist_vdecl = next tokenlist in
                let (tokenlist_vdecl_list, vdecl_list) = parseVdeclList tokenlist_vdecl in 
                (tokenlist_vdecl_list, Ast.VdeclList(vdecl_list))
    | Lexer.LeftParens -> (tokenlist, Ast.VdeclListEmpty)
    | Lexer.Return -> (tokenlist, Ast.VdeclListEmpty)
    | Lexer.LeftBrace -> (tokenlist, Ast.VdeclListEmpty)
    | Lexer.RightBrace -> (tokenlist, Ast.VdeclListEmpty)
    | Lexer.If -> (tokenlist, Ast.VdeclListEmpty)
    | Lexer.For -> (tokenlist, Ast.VdeclListEmpty)
    | Lexer.While -> (tokenlist, Ast.VdeclListEmpty)
    | Lexer.Numeral str -> (tokenlist, Ast.VdeclListEmpty)
    | Lexer.Minus -> (tokenlist, Ast.VdeclListEmpty)
    | Lexer.Not -> (tokenlist, Ast.VdeclListEmpty)
    | Lexer.ID identifier -> (tokenlist, Ast.VdeclListEmpty)
    | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
        raise (Syntax_error err_msg)


(* expr = T EPRIME
*)
let rec parseExpr tokenlist =
    match tokenlist.head with 
    | Lexer.LeftParens -> let (tokenlist_t, t_expr) = next tokenlist |> parseT in 
                  let (tokenlist_e, e_expr) = parseEPrime tokenlist_t in
                  (tokenlist_e, Ast.Expression(t_expr, e_expr))
    | Lexer.Numeral str -> let (tokenlist_t, t_expr) = next tokenlist |> parseT in 
                  let (tokenlist_e, e_expr) = parseEPrime tokenlist_t in
                  (tokenlist_e, Ast.Expression(t_expr, e_expr))
    | Lexer.True -> let (tokenlist_t, t_expr) = next tokenlist |> parseT in 
                  let (tokenlist_e, e_expr) = parseEPrime tokenlist_t in
                  (tokenlist_e, Ast.Expression(t_expr, e_expr))
    | Lexer.False -> let (tokenlist_t, t_expr) = next tokenlist |> parseT in 
                  let (tokenlist_e, e_expr) = parseEPrime tokenlist_t in
                  (tokenlist_e, Ast.Expression(t_expr, e_expr))
    | Lexer.ID identifier -> let (tokenlist_t, t_expr) = next tokenlist |> parseT in 
                  let (tokenlist_e, e_expr) = parseEPrime tokenlist_t in
                  (tokenlist_e, Ast.Expression(t_expr, e_expr))
    | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
          raise (Syntax_error err_msg)

(* F -> LPAREN E RPAREN 
F -> int_literal 
F -> TRUE 
F -> FALSE
F -> ID*)
and parseF tokenlist = 
  begin
  match tokenlist.head with
  | Lexer.LeftParens -> let (tokenlist_expr, expr) = next tokenlist |> parseExpr in 
                        let tokenlist_expr_next = next tokenlist_expr in
                          begin
                          match tokenlist_expr_next.head with 
                          | Lexer.RightParens -> (next tokenlist_expr_next, Ast.ExpressionParen(expr))
                          | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                                raise (Syntax_error err_msg)
                          end
  | Lexer.Numeral str-> (next tokenlist, Ast.Literal(str))
  | Lexer.True -> (next tokenlist, Ast.BoolLit(true))
  | Lexer.False -> (next tokenlist, Ast.BoolLit(false))
  | Lexer.ID identifier -> (next tokenlist, Ast.Id(identifier))
  | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
        raise (Syntax_error err_msg)
   end

(*T* -> * F T* 
T* -> / F T* 
T* -> ε *)
and parseTprime tokenlist = 
  begin
  match  tokenlist.head with
  | Lexer.Multiply -> let (tokenlist_f, expr_f) = next tokenlist |> parseF in 
                let (tokenlist_tprime, expr_tprime) = parseTprime tokenlist_f in 
                (tokenlist_tprime, Ast.Times(expr_f, expr_tprime))
  | Lexer.Divide -> let (tokenlist_f, expr_f) = next tokenlist |> parseF in 
                let (tokenlist_tprime, expr_tprime) = parseTprime tokenlist_f in 
                (tokenlist_tprime, Ast.Divide(expr_f, expr_tprime))
  | Lexer.Plus -> (tokenlist, Ast.TPempty)
  | Lexer.Minus -> (tokenlist, Ast.TPempty)
  | Lexer.Semicolon -> (tokenlist, Ast.TPempty)
  | Lexer.RightParens -> (tokenlist, Ast.TPempty)
  | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
        raise (Syntax_error err_msg)
  end
(* T -> F T* *)
and parseT tokenlist = 
  begin
  match tokenlist.head with 
  | Lexer.LeftParens -> let (tokenlist_f, expr_f) = parseF tokenlist in 
                let (tokenlist_tprime, expr_tprime) = parseTprime tokenlist_f in 
                (tokenlist_tprime, Ast.F(expr_f, expr_tprime))
  | Lexer.Numeral str -> let (tokenlist_f, expr_f) = parseF tokenlist in 
                let (tokenlist_tprime, expr_tprime) = parseTprime tokenlist_f in 
                (tokenlist_tprime, Ast.F(expr_f, expr_tprime))
  | Lexer.True -> let (tokenlist_f, expr_f) = parseF tokenlist in 
                let (tokenlist_tprime, expr_tprime) = parseTprime tokenlist_f in 
                (tokenlist_tprime, Ast.F(expr_f, expr_tprime))
  | Lexer.False -> let (tokenlist_f, expr_f) = parseF tokenlist in 
                let (tokenlist_tprime, expr_tprime) = parseTprime tokenlist_f in 
                (tokenlist_tprime, Ast.F(expr_f, expr_tprime))
  | Lexer.ID identifier -> let (tokenlist_f, expr_f) = parseF tokenlist in 
                let (tokenlist_tprime, expr_tprime) = parseTprime tokenlist_f in 
                (tokenlist_tprime, Ast.F(expr_f, expr_tprime))
  | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
        raise (Syntax_error err_msg)
  end
(*expr_prime = E* -> + T E* 
E* -> - T E* 
E* -> ε
*)
and parseEPrime tokenlist =
  match tokenlist.head with
   | Lexer.Plus -> let (tokenlist_t, expr_t) = next tokenlist |> parseT in
                let (tokenlist_e, expr_eprime) = parseEPrime tokenlist_t in 
                (tokenlist_e, Ast.Add(expr_t, expr_eprime))
   | Lexer.Minus -> let (tokenlist_t, expr_t) = next tokenlist |> parseT in
                let (tokenlist_e, expr_eprime) = parseEPrime tokenlist_t in 
                (tokenlist_e, Ast.Minus(expr_t, expr_eprime))
   | Lexer.Semicolon -> (tokenlist, Ast.EPrimeEmpty)
   | Lexer.RightParens -> (tokenlist,Ast.EPrimeEmpty)
   | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
         raise (Syntax_error err_msg)


(* assignmentType -> typ ASSIGN expr 
assignmentType -> ASSSIGN expr*)
let parseAssignmentType tokenlist = 
  match tokenlist.head with
  |Lexer.Equality -> let (tokenlist_expr, expr) = next tokenlist |> parseExpr in 
                (tokenlist_expr, Ast.Assign(expr))
  |Lexer.Int -> let (tokenlist_typ, typ) = parseTyp tokenlist in 
            begin
            match tokenlist_typ.head with
            | Lexer.Equality -> let (tokenlist_expr, expr) = next tokenlist |> parseExpr in 
                          (tokenlist_expr, Ast.TypeAssign(typ, expr))
            | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                  raise (Syntax_error err_msg)
            end
  |Lexer.Bool -> let (tokenlist_typ, typ) = parseTyp tokenlist in 
            begin
            match tokenlist_typ.head with
            | Lexer.Equality -> let (tokenlist_expr, expr) = next tokenlist |> parseExpr in 
                          (tokenlist_expr, Ast.TypeAssign(typ, expr))
            | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                  raise (Syntax_error err_msg)
            end
  |Lexer.Void -> let (tokenlist_typ, typ) = parseTyp tokenlist in
            begin 
            match tokenlist_typ.head with
            | Lexer.Equality -> let (tokenlist_expr, expr) = next tokenlist |> parseExpr in 
                          (tokenlist_expr, Ast.TypeAssign(typ, expr))
            | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                  raise (Syntax_error err_msg)
            end
  | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
        raise (Syntax_error err_msg)

(* assignment -> ID assignmentType
assignment -> expr
assignmentType -> typ ASSIGN expr 
assignmentType -> ASSSIGN expr*)
let parseAssignment tokenlist = 
  match tokenlist.head with
  |Lexer.ID identifier -> let (tokenlist_assignment, assignmentType) = next tokenlist |> parseAssignmentType in 
            (tokenlist_assignment, Ast.IA(identifier, assignmentType))
  |Lexer.LeftParens -> let (tokenlist_expr, expr) = parseExpr tokenlist in 
              (tokenlist_expr, Ast.AExpr(expr))
  |Lexer.Numeral str -> let (tokenlist_expr, expr) = parseExpr tokenlist in 
              (tokenlist_expr, Ast.AExpr(expr))
  |Lexer.True -> let (tokenlist_expr, expr) = parseExpr tokenlist in 
              (tokenlist_expr, Ast.AExpr(expr))
  |Lexer.False -> let (tokenlist_expr, expr) = parseExpr tokenlist in 
              (tokenlist_expr, Ast.AExpr(expr))
  | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
          raise (Syntax_error err_msg) 

(*stmt_prime ->SEMI| expr SEMI*)
let parseStmtOpt tokenlist = 
     match tokenlist.head with 
     | Lexer.Semicolon -> (next tokenlist, Ast.OptNil)
     | Lexer.LeftParens -> let (tokenlist_expr, expr) = next tokenlist |> parseAssignment in 
                      begin
                      match tokenlist_expr.head with 
                      |Lexer.Semicolon -> (next tokenlist_expr, Ast.StmtExpression(expr)) 
                      | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                            raise (Syntax_error err_msg) 
                      end
     | Lexer.Numeral str -> let (tokenlist_expr, expr) = next tokenlist |> parseAssignment in 
                      begin
                      match tokenlist_expr.head with 
                      |Lexer.Semicolon -> (next tokenlist_expr, Ast.StmtExpression(expr))
                      | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                            raise (Syntax_error err_msg) 
                      end
     | Lexer.ID identifier -> let (tokenlist_expr, expr) = next tokenlist |> parseAssignment in 
                      begin
                      match tokenlist_expr.head with 
                      |Lexer.Semicolon -> (next tokenlist_expr, Ast.StmtExpression(expr)) 
                      | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                          raise (Syntax_error err_msg)
                      end 
     | Lexer.True -> let (tokenlist_expr, expr) = next tokenlist |> parseAssignment in 
                      begin
                      match tokenlist_expr.head with 
                      |Lexer.Semicolon -> (next tokenlist_expr, Ast.StmtExpression(expr))
                      | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                             raise (Syntax_error err_msg) 
                      end
     | Lexer.False -> let (tokenlist_expr, expr) = next tokenlist |> parseAssignment in 
                      begin
                      match tokenlist_expr.head with 
                      |Lexer.Semicolon -> (next tokenlist_expr, Ast.StmtExpression(expr))
                      | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                          raise (Syntax_error err_msg) 
                      end
     | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
            raise (Syntax_error err_msg) 


(*stmt_list = stmt stmt_list | epsilon*)
let rec parseStmtList tokenlist =
    match tokenlist.head with 
    | Lexer.Semicolon -> let (tokenlist_stmt, stmt) = parseStmt tokenlist in 
                        let (tokenlist_stmt_list, stmt_list) = parseStmtList tokenlist_stmt in 
                        (tokenlist_stmt_list, Ast.StmtList( stmt, stmt_list))
    | Lexer.LeftParens -> let (tokenlist_stmt, stmt) = parseStmt tokenlist in 
                let (tokenlist_stmt_list, stmt_list) = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList(stmt, stmt_list))
    | Lexer.Return -> let (tokenlist_stmt, stmt) = parseStmt tokenlist in 
                let (tokenlist_stmt_list, stmt_list) = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList(stmt, stmt_list))
    | Lexer.LeftBrace -> let (tokenlist_stmt, stmt) = parseStmt tokenlist in 
                let (tokenlist_stmt_list, stmt_list) = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList(stmt, stmt_list))
    | Lexer.RightBrace -> (tokenlist, Ast.StmtlistNil)
    | Lexer.If -> let (tokenlist_stmt, stmt) = parseStmt tokenlist in 
                let (tokenlist_stmt_list, stmt_list) = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList(stmt, stmt_list))
    | Lexer.For -> let (tokenlist_stmt, stmt) = parseStmt tokenlist in 
                let (tokenlist_stmt_list, stmt_list) = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList(stmt, stmt_list))
    | Lexer.While -> let (tokenlist_stmt, stmt) = parseStmt tokenlist in 
                let (tokenlist_stmt_list, stmt_list) = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList(stmt, stmt_list))
    | Lexer.Numeral str -> let (tokenlist_stmt, stmt) = parseStmt tokenlist in  
                let (tokenlist_stmt_list, stmt_list) = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList(stmt, stmt_list))
    | Lexer.Minus -> let (tokenlist_stmt, stmt) = parseStmt tokenlist in  
                let (tokenlist_stmt_list, stmt_list) = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList(stmt, stmt_list))
    | Lexer.ID identifier -> let (tokenlist_stmt, stmt) = parseStmt tokenlist in  
                let (tokenlist_stmt_list, stmt_list) = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList(stmt, stmt_list))
    | Lexer.True -> let (tokenlist_stmt, stmt) = parseStmt tokenlist in 
                let (tokenlist_stmt_list, stmt_list) = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList(stmt, stmt_list))
    | Lexer.False -> let (tokenlist_stmt, stmt) = parseStmt tokenlist in  
                let (tokenlist_stmt_list, stmt_list) = parseStmtList tokenlist_stmt in 
                (tokenlist_stmt_list, Ast.StmtList(stmt, stmt_list))
    | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
          raise (Syntax_error err_msg)

(*stmt -> assignment SEMI 
|  RETURN stmt_opt 
|  LBRACE stmt_list RBRACE 
|  IF LPAREN assignment RPAREN stmt 
|  FOR LPAREN assignment SEMI assignment SEMI assignment RPAREN stmt  
|  WHILE LPAREN assignment RPAREN stmt
*)

and parseStmt tokenlist = 
   begin
   match tokenlist.head with 
   | Lexer.ID identifier -> let (tokenlist_assignment, assignment) = parseAssignment tokenlist in
                begin
                match tokenlist_assignment.head with
                | Lexer.Semicolon -> (tokenlist_assignment, Ast.Assignment(assignment))
                | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                     raise (Syntax_error err_msg) 
                end
   |  Lexer.LeftParens -> let (tokenlist_assignment, assignment) = parseAssignment tokenlist in
                begin
                match tokenlist_assignment.head with
                | Lexer.Semicolon -> (tokenlist_assignment, Ast.Assignment(assignment))
                | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                     raise (Syntax_error err_msg) 
                 end
   | Lexer.Numeral str -> let (tokenlist_assignment, assignment) = parseAssignment tokenlist in
                begin
                match tokenlist_assignment.head with
                | Lexer.Semicolon -> (tokenlist_assignment, Ast.Assignment(assignment))
                | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                     raise (Syntax_error err_msg) 
                end
   | Lexer.True -> let (tokenlist_assignment, assignment) = parseAssignment tokenlist in
                begin
                match tokenlist_assignment.head with
                | Lexer.Semicolon -> (tokenlist_assignment, Ast.Assignment(assignment))
                | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                     raise (Syntax_error err_msg) 
                end
   | Lexer.False -> let (tokenlist_assignment, assignment) = parseAssignment tokenlist in
                begin
                match tokenlist_assignment.head with
                | Lexer.Semicolon -> (tokenlist_assignment, Ast.Assignment(assignment))
                | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                     raise (Syntax_error err_msg) 
                end
   | Lexer.Return -> let (tokenlist_stmt_opt, stmt_prime) = next tokenlist |> parseStmtOpt in 
                 (tokenlist_stmt_opt, Ast.Return(stmt_prime))
   | Lexer.If -> let tokenlist_lparen = next tokenlist |> next in 
             begin
             match tokenlist_lparen.head with 
             | Lexer.LeftParens -> let (tokenlist_assignment, expr) = next tokenlist_lparen |> parseAssignment in 
                                   begin
                                    match tokenlist_assignment.head with 
                                  | Lexer.RightParens -> let (tokenlist_stmt, stmt) = next tokenlist_assignment |> parseStmt in 
                                                        (tokenlist_stmt, Ast.If(expr, stmt)) 
                                  | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                                        raise (Syntax_error err_msg)
                                  end
             | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                    raise (Syntax_error err_msg)
              end
   | Lexer.For -> let nexthead = next tokenlist in
            begin
            match nexthead.head with
            | Lexer.LeftParens -> let (tokenlist_assignment, expr) = parseAssignment nexthead in 
                          begin
                          match tokenlist_assignment.head with
                          | Lexer.Semicolon -> let (tokenlist_assignment2, expr2) = next tokenlist_assignment |> parseAssignment in
                                      begin
                                      match tokenlist_assignment2.head with 
                                      | Lexer.Semicolon -> let (tokenlist_assignment3, expr3) = next tokenlist_assignment2 |> parseAssignment in 
                                                  begin
                                                  match  tokenlist_assignment3.head with
                                                  | Lexer.RightParens -> let (tokenlist_stmt, stmt) = next tokenlist_assignment3 |> parseStmt in
                                                  (tokenlist_stmt, Ast.For(expr, expr2, expr3, stmt))
                                                  | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                                                        raise (Syntax_error err_msg)
                                                  end
                                      | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                                              raise (Syntax_error err_msg)
                                      end
                          | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                                  raise (Syntax_error err_msg)
                          end
              | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                                  raise (Syntax_error err_msg)
              end
   | Lexer.While -> let tokenlist_lparen = next tokenlist in 
                begin
                match tokenlist_lparen.head with
                | Lexer.LeftParens -> let (tokenlist_assignment, assignment) = next tokenlist_lparen |> parseAssignment in
                              begin
                              match tokenlist_assignment.head with
                              | Lexer.RightParens -> let (tokenlist_stmt, stmt) = next tokenlist_assignment |> parseStmt in
                                            (next tokenlist_stmt, Ast.While(assignment, stmt))
                              | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                                    raise (Syntax_error err_msg)
                              end
                | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                    raise (Syntax_error err_msg)
                end
   | Lexer.LeftBrace -> let tokenlist_leftbrace = next tokenlist in 
                        let (tokenlist_expr, expr) = parseStmtList tokenlist_leftbrace in
                        begin
                        match tokenlist_expr.head with
                        | Lexer.RightBrace -> (next tokenlist_expr, Ast.Parentheses(expr))
                        | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                              raise (Syntax_error err_msg)
                        end
   | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                    raise (Syntax_error err_msg)
    end


(* Fdecl = “lparen” formals_opt “rparen” “LBRACE” vdecl_list stmt_list “RBRACE” *)
let parseFdecl tokenlist = 
    match tokenlist.head with 
    | Lexer.LeftParens -> let (tokenlist_formals, formals_opt) = next tokenlist |> parseFormalsOpt  in 
                  begin
                  match tokenlist_formals.head with 
                  | Lexer.RightParens -> let tokenlist_rparen = next tokenlist_formals in
                                begin
                                match tokenlist_rparen.head with 
                                | Lexer.LeftBrace -> let (tokenlist_vdecl_list, vdecl_list) = next tokenlist_formals |> parseVdeclList  in 
                                              let (tokenlist_stmt_list, stmt_list) = parseStmtList tokenlist_vdecl_list in 
                                              begin
                                              match tokenlist_stmt_list.head with 
                                              | Lexer.RightBrace -> (next tokenlist_stmt_list, Ast.Funcdecl(formals_opt, vdecl_list, stmt_list))
                                              | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                                                      raise (Syntax_error err_msg)
                                              end
                                | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                                      raise (Syntax_error err_msg)
                                end
                  | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                        raise (Syntax_error err_msg)
                  end
    | _-> let err_msg = __LOC__ ^ " Syntax Error in parseFdecl, left parents expected, received:" ^ Lexer.show_token tokenlist.head in
        raise (Syntax_error err_msg)

(*decls = typ “id” decls_prime | epsilon *)
let rec parseDecls tokenlist = 
    match tokenlist.head with 
    | Lexer.Int -> let (tokenlist_typ, typ) = parseTyp tokenlist in 
                   begin
                   match tokenlist_typ.head with 
                   | Lexer.ID identifier -> let (tokenlist_decls_prime, decls_prime) = next tokenlist_typ |> parseDeclsPrime in 
                                (tokenlist_decls_prime, Ast.Declaration(typ, identifier, decls_prime))
                   | _-> let err_msg = Printf.sprintf "Syntax Error" in
                          raise (Syntax_error err_msg)
                   end
    | Lexer.Bool -> let (tokenlist_typ, typ) = parseTyp tokenlist in 
                   begin
                   match tokenlist_typ.head with 
                   | Lexer.ID identifier -> let (tokenlist_decls_prime, decls_prime) = next tokenlist_typ |> parseDeclsPrime in 
                                (tokenlist_decls_prime, Ast.Declaration(typ, identifier, decls_prime))
                   | _-> let err_msg = Printf.sprintf "Syntax Error" in
                          raise (Syntax_error err_msg)
                   end
    | Lexer.Void -> let (tokenlist_typ, typ) = parseTyp tokenlist in 
                   begin
                   match tokenlist_typ.head with 
                   | Lexer.ID identifier -> let (tokenlist_decls_prime, decls_prime) = next tokenlist_typ |> parseDeclsPrime in 
                                (tokenlist_decls_prime, Ast.Declaration(typ, identifier, decls_prime))
                   | _-> let err_msg = Printf.sprintf "Syntax Error" in
                          raise (Syntax_error err_msg)
                   end
    | Lexer.EOF -> (tokenlist, Ast.DEmpty)
    | _-> let err_msg = Printf.sprintf "Syntax Error" in
           raise (Syntax_error err_msg) 
     

(* decls_prime = vdecl decls | fdecl decls *)
and parseDeclsPrime tokenlist =
    begin
    match tokenlist.head with 
    | Lexer.Semicolon -> let tokenlist_vdecl = next tokenlist in
                let (tokenlist_decls, decls) = parseDecls tokenlist_vdecl in 
                (tokenlist_decls, Ast.Vdecl(decls)) 
    | Lexer.LeftParens -> let (tokenlist_fdecl, fdecl) = tokenlist |> parseFdecl in 
                let (tokenlist_decls, decls) = parseDecls tokenlist_fdecl in 
                (tokenlist_decls, Ast.Fdecl(fdecl, decls)) 
    | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
          raise (Syntax_error err_msg)
    end



(*Program = decls “EOF”*)
let parseProgram tokenlist = 
    match tokenlist.head with 
    | Lexer.Int -> let (tokenlist_decls, decls) = parseDecls tokenlist in 
               begin
               match tokenlist_decls.head with
               | Lexer.EOF -> ([], Ast.Program decls)
               | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                    raise (Syntax_error err_msg)
               end
    | Lexer.Bool -> let (tokenlist_decls, decls) = parseDecls tokenlist in 
               begin
               match tokenlist_decls.head with
               | Lexer.EOF -> ([], Ast.Program decls)
               | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                    raise (Syntax_error err_msg)
               end
    | Lexer.Void -> let (tokenlist_decls, decls) = parseDecls tokenlist in 
               begin
               match tokenlist_decls.head with
               | Lexer.EOF -> ([], Ast.Program decls)
               | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
                    raise (Syntax_error err_msg)
                end
    | Lexer.EOF -> (tokenlist.lexbuf, Ast.ProgramNil)
    | _-> let err_msg = __LOC__ ^ "Syntax Error " ^ show_token_list tokenlist in
          raise (Syntax_error err_msg)

