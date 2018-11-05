(** Parser.
    A [Parser] is a LL(1), top-down recursive descent parser for PL/0 language. 
    https://en.wikipedia.org/wiki/PL/0. *)
open Lexer
open Ast

exception Syntax_error of string 
(** Raised when [!Parser] encounters an unrecongnized token. *)

type token_list = 
  {lookahead : Lexer.token; (** look-ahead token. *)
   lexbuf : Lexing.lexbuf}  (** lexer buffer. *)
(** Represents a parser buffer used during parsing of various productions. *)

let default_tokenlist s = {lookahead = Lexer.Eof; lexbuf = Lexing.from_string s}
(** Create a default [token_list] with the given string [s]. *)

let next tokenlist = {pb with lookahead = Lexer.next_token pb.lexbuf}
(** Retrieves a new parser buffer with the next lookahead token. *)

let expect_error tokenlist t fname =
  let la_str = Lexer.to_string tokenlist.lookahead and e_str = Lexer.to_string t in
  let err_msg = Printf.sprintf "Syntax Error. Expected token '%s', however received '%s' in %s().\n" e_str la_str fname in 
  let e = Syntax_error err_msg in
  raise e

(** Throws [Parse_error]. *)
let error tokenlist fname =
  let la_str = Lexer.to_string tokenlist.lookahead in 
  let err_msg = Printf.sprintf "Syntax Error. Unexpected token '%s'\n" la_str in
  let e = Syntax_error err_msg in
  raise e            

let is_same t1 t2 =
  match t1, t2 with 
  | Ident _, Ident _ -> true
  | Number _, Number _ -> true
  | a, b when a = b -> true
  | _ , _ -> false
(** Returns [true] if two [Lexer.token]s are the same type, [false] otherwise. *)

let is_token_in l t = List.exists (is_same t) l
(** Returns true if token 't' is in [Lexer.token list] 'l'. false otherwise. *)

let expect t tokenlist =
  let expected = is_same t tokenlist.lookahead in
  if expected then (next tokenlist, expected) else (pb, expected)

(** Expects the given token [t] to match the [tokenlist.lookahead] token in [pb]. Raises 'Syntax_error' exception
    if the two tokens donot match. *)


(* actuals_opt = actuals_list | epsilon*)
let rec parse_actuals_opt tokenlist = parse_actuals_list tokenlist

(* actuals_list = expr actuals_list_prime*)
and parse_actuals_list tokenlist = let(tokenlist, e) = parse_expr tokenlist in 
				   let(tokenlist, x) = parse_actuals_list_prime tokenlist in
				   let(tokenlist, expected) = expect Comma tokenlist in 
                                   if expected then (tokenlist, Actual(e)) else expect_error pb Comma "parse_Actuals_list"
(* how to account for the epsilon that actuals_list_prime could return??*)

(*Actuals_list_prime = COMMA expr actuals_list_prime | epsilon *)
and parse_actuals_list_prime tokenlist = 
  match tokenlist.lookahead with 
  | Lexer.Comma c -> let(tokenlist, e) = parse_expr next tokenlist in
                     let(tokenlist, t) = parse_actuals_list_prime tokenlist 
		     (*when do we return??? have to figure out epsilon here too*)

(* Expr =  expr expr_prime_prime
        | ID expr_prime
        | LITERAL
        | TRUE
        | FALSE
        | MINUS expr %prec NEG
        | NOT expr
        | LPAREN expr RPAREN *)
let rec parse_expr tokenlist = 
  match tokenlist.lookahead with 
  | Lexer.Ident id -> let(tokenlist, e) = parse_expr_prime next tokenlist in (tokenlist, Ast.id(???))
  | Lexer.Literal num -> (next tokenlist, Literal num)
  | Lexer.True tru -> (next tokenlist, BoolLit tru)
  | Lexer.False fal -> (next tokenlist, BoolLit fal)
  | Lexer.Minus -> 
      let(tokenlist, e) = parse_expr next tokenlist in
      let(tokenlist, expected) = expect Neg tokenlist in 
      if expected then (tokenlist, Unop(Neg, e)) else expect_error tokenlist Neg "parse_expr"   

(*expr_prime = ϵ | ASSIGN expr | LPAREN actuals_opt RPAREN *)
and parse_expr_prime tokenlist = 
  match tokenlist.lookahead with 
  | Lexer.Assign x -> let(tokenlist, e) = parse_expr next tokenlist in (tokenlist, Ast.Assign(e))
  | Lexer.Lparen l -> 
      let(tokenlist, e) = parse_actuals_opt next tokenlist in 
      let(tokenlist, expected) = expect Rparen tokenlist in 
      if expected then (tokenlist, Expr e) else expect_error tokenlist Rparen "parse_expr_prime"
      (* have to figure out epsilon*)

(*Expr_prime_prime = PLUS expr| MINUS expr| TIMES expr| DIVIDE expr| EQ expr| NEQ expr| LT expr| LEQ expr| GT expr| GEQ expr| AND expr| OR expr *)
and parse_expr_prime_prime tokenlist = 
    match tokenlist.lookahead with 
    | Lexer.Plus x -> let(tokenlist, e) = parse_expr next tokenlist in (tokenlist, Ast.Plus(e))
    | Lexer.Minus x -> let(tokenlist, e) = parse_expr next tokenlist in (tokenlist, Ast.Minus(e))
    | Lexer.Times x -> let(tokenlist, e) = parse_expr next tokenlist in (tokenlist, Ast.Times(e))
    | Lexer.Divide x -> let(tokenlist, e) = parse_expr next tokenlist in (tokenlist, Ast.Divide(e))
    | Lexer.EQ x -> let(tokenlist, e) = parse_expr next tokenlist in (tokenlist, Ast.EQ(e))
    | Lexer.NEQ x -> let(tokenlist, e) = parse_expr next tokenlist in (tokenlist, Ast.NEQ(e))
    | Lexer.LT x -> let(tokenlist, e) = parse_expr next tokenlist in (tokenlist, Ast.LT(e))
    | Lexer.LEQ x -> let(tokenlist, e) = parse_expr next tokenlist in (tokenlist, Ast.LEQ(e))
    | Lexer.GT x -> let(tokenlist, e) = parse_expr next tokenlist in (tokenlist, Ast.GT(e))
    | Lexer.GEQ x -> let(tokenlist, e) = parse_expr next tokenlist in (tokenlist, Ast.GEQ(e))
    | Lexer.AND x -> let(tokenlist, e) = parse_expr next tokenlist in (tokenlist, Ast.AND(e))
    | Lexer.OR x -> let(tokenlist, e) = parse_expr next tokenlist in (tokenlist, Ast.AND(e))
   

(* program = decls . *)
let program tokenlist = 
  let (tokenlist, decls_tree) = parse_decls tokenlist in 
  if tokenlist.lookahead = Lexer.Period then Program(decls_tree)
  else expect_error tokenlist Lexer.Period "program"


(* Main entry point to the parser. *)
let parse_csomething lb =
  let tokenlist = {lookahead = Lexer.Eof; lexbuf = lb}
  in
  program tokenlist
