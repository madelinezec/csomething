type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | TRUE
  | FALSE
  | AND
  | OR
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | BOOL
  | VOID
  | MAT
  | FLOAT
  | LITERAL of (int)
  | NUMBER of (float)
  | ID of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 3 "src/parser.mly"
open Ast
# 46 "src/parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* LBRACKET *);
  263 (* RBRACKET *);
  264 (* COMMA *);
  265 (* PLUS *);
  266 (* MINUS *);
  267 (* TIMES *);
  268 (* DIVIDE *);
  269 (* ASSIGN *);
  270 (* NOT *);
  271 (* EQ *);
  272 (* NEQ *);
  273 (* LT *);
  274 (* LEQ *);
  275 (* GT *);
  276 (* GEQ *);
  277 (* TRUE *);
  278 (* FALSE *);
  279 (* AND *);
  280 (* OR *);
  281 (* RETURN *);
  282 (* IF *);
  283 (* ELSE *);
  284 (* FOR *);
  285 (* WHILE *);
  286 (* INT *);
  287 (* BOOL *);
  288 (* VOID *);
  289 (* MAT *);
  290 (* FLOAT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  291 (* LITERAL *);
  292 (* NUMBER *);
  293 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\006\000\010\000\
\010\000\009\000\009\000\005\000\005\000\005\000\005\000\005\000\
\007\000\007\000\003\000\008\000\008\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\013\000\013\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\014\000\014\000\015\000\
\015\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\000\000\001\000\002\000\
\005\000\001\000\003\000\001\000\001\000\001\000\001\000\001\000\
\000\000\002\000\002\000\000\000\002\000\002\000\002\000\003\000\
\003\000\005\000\007\000\009\000\005\000\000\000\001\000\001\000\
\001\000\001\000\001\000\001\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\002\000\002\000\003\000\004\000\003\000\000\000\001\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\058\000\000\000\012\000\013\000\014\000\015\000\
\016\000\001\000\003\000\004\000\000\000\000\000\000\000\019\000\
\000\000\000\000\000\000\000\000\000\000\010\000\000\000\000\000\
\000\000\000\000\009\000\017\000\011\000\000\000\018\000\000\000\
\000\000\020\000\005\000\000\000\000\000\034\000\035\000\000\000\
\000\000\000\000\000\000\032\000\033\000\000\000\021\000\000\000\
\000\000\000\000\049\000\050\000\023\000\000\000\000\000\000\000\
\000\000\000\000\000\000\022\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\053\000\025\000\024\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\039\000\040\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\052\000\000\000\000\000\000\000\029\000\000\000\
\000\000\000\000\027\000\000\000\000\000\028\000"

let yydgoto = "\002\000\
\003\000\004\000\011\000\012\000\019\000\020\000\030\000\032\000\
\021\000\014\000\047\000\048\000\078\000\081\000\082\000"

let yysindex = "\020\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\246\254\041\255\053\255\000\000\
\080\255\013\255\007\255\065\255\046\255\000\000\049\255\064\255\
\075\255\080\255\000\000\000\000\000\000\080\255\000\000\067\255\
\188\255\000\000\000\000\188\255\188\255\000\000\000\000\039\255\
\081\255\085\255\088\255\000\000\000\000\000\255\000\000\238\255\
\189\000\095\255\000\000\000\000\000\000\002\000\188\255\188\255\
\188\255\188\255\188\255\000\000\188\255\188\255\188\255\188\255\
\188\255\188\255\188\255\188\255\188\255\188\255\188\255\188\255\
\000\000\000\000\000\000\207\000\241\000\093\255\225\000\241\000\
\079\255\090\255\241\000\051\255\051\255\000\000\000\000\028\001\
\028\001\129\255\129\255\129\255\129\255\016\001\001\001\179\255\
\188\255\179\255\000\000\188\255\091\255\027\000\000\000\241\000\
\179\255\188\255\000\000\105\255\179\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\114\255\000\000\
\116\255\000\000\000\000\000\000\119\255\000\000\000\000\008\255\
\000\000\000\000\000\000\000\000\000\000\123\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\218\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\125\255\
\000\000\126\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\017\255\000\000\000\000\043\255\
\000\000\131\255\009\255\051\000\075\000\000\000\000\000\042\255\
\105\000\099\000\123\000\147\000\171\000\083\255\077\255\000\000\
\000\000\000\000\000\000\000\000\151\255\000\000\000\000\044\255\
\000\000\132\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\106\000\000\000\138\000\000\000\000\000\109\000\
\000\000\244\255\166\255\223\255\040\000\000\000\000\000"

let yytablesize = 560
let yytable = "\049\000\
\010\000\058\000\051\000\052\000\022\000\101\000\054\000\103\000\
\008\000\051\000\008\000\051\000\059\000\029\000\107\000\008\000\
\051\000\031\000\110\000\031\000\001\000\076\000\077\000\079\000\
\080\000\083\000\015\000\084\000\085\000\086\000\087\000\088\000\
\089\000\090\000\091\000\092\000\093\000\094\000\095\000\053\000\
\033\000\016\000\041\000\024\000\041\000\056\000\057\000\023\000\
\036\000\041\000\056\000\057\000\037\000\026\000\017\000\027\000\
\041\000\041\000\018\000\038\000\039\000\063\000\064\000\102\000\
\041\000\041\000\104\000\025\000\033\000\018\000\034\000\035\000\
\077\000\044\000\045\000\046\000\036\000\048\000\028\000\048\000\
\037\000\099\000\055\000\047\000\048\000\047\000\056\000\038\000\
\039\000\057\000\047\000\040\000\041\000\097\000\042\000\043\000\
\033\000\100\000\034\000\074\000\048\000\044\000\045\000\046\000\
\036\000\047\000\047\000\109\000\037\000\005\000\006\000\007\000\
\008\000\009\000\008\000\038\000\039\000\105\000\006\000\040\000\
\041\000\007\000\042\000\043\000\020\000\030\000\020\000\020\000\
\054\000\044\000\045\000\046\000\020\000\055\000\030\000\031\000\
\020\000\061\000\062\000\063\000\064\000\013\000\050\000\020\000\
\020\000\108\000\000\000\020\000\020\000\000\000\020\000\020\000\
\026\000\000\000\026\000\026\000\000\000\020\000\020\000\020\000\
\026\000\000\000\000\000\000\000\026\000\000\000\000\000\000\000\
\000\000\000\000\000\000\026\000\026\000\000\000\000\000\026\000\
\026\000\000\000\026\000\026\000\033\000\000\000\034\000\000\000\
\000\000\026\000\026\000\026\000\036\000\033\000\000\000\000\000\
\037\000\000\000\000\000\000\000\000\000\036\000\000\000\038\000\
\039\000\037\000\000\000\040\000\041\000\000\000\042\000\043\000\
\038\000\039\000\000\000\000\000\000\000\044\000\045\000\046\000\
\000\000\000\000\036\000\000\000\036\000\000\000\044\000\045\000\
\046\000\036\000\036\000\036\000\036\000\036\000\000\000\000\000\
\036\000\036\000\036\000\036\000\036\000\036\000\060\000\000\000\
\036\000\036\000\000\000\000\000\000\000\000\000\061\000\062\000\
\063\000\064\000\000\000\000\000\065\000\066\000\067\000\068\000\
\069\000\070\000\075\000\000\000\071\000\072\000\000\000\000\000\
\000\000\000\000\061\000\062\000\063\000\064\000\000\000\000\000\
\065\000\066\000\067\000\068\000\069\000\070\000\000\000\000\000\
\071\000\072\000\000\000\106\000\000\000\000\000\005\000\006\000\
\007\000\008\000\009\000\061\000\062\000\063\000\064\000\000\000\
\000\000\065\000\066\000\067\000\068\000\069\000\070\000\000\000\
\000\000\071\000\072\000\037\000\000\000\037\000\000\000\000\000\
\000\000\000\000\037\000\037\000\037\000\000\000\000\000\000\000\
\000\000\037\000\037\000\037\000\037\000\037\000\037\000\000\000\
\000\000\037\000\037\000\038\000\000\000\038\000\000\000\000\000\
\000\000\000\000\038\000\038\000\038\000\000\000\000\000\000\000\
\000\000\038\000\038\000\038\000\038\000\038\000\038\000\000\000\
\000\000\038\000\038\000\043\000\000\000\043\000\000\000\000\000\
\000\000\042\000\043\000\042\000\000\000\000\000\000\000\000\000\
\042\000\043\000\043\000\043\000\043\000\043\000\043\000\042\000\
\042\000\043\000\043\000\044\000\000\000\044\000\000\000\042\000\
\042\000\000\000\044\000\000\000\000\000\000\000\000\000\000\000\
\000\000\044\000\044\000\044\000\044\000\044\000\044\000\000\000\
\000\000\044\000\044\000\045\000\000\000\045\000\000\000\000\000\
\000\000\000\000\045\000\000\000\000\000\000\000\000\000\000\000\
\000\000\045\000\045\000\045\000\045\000\045\000\045\000\000\000\
\000\000\045\000\045\000\046\000\000\000\046\000\000\000\000\000\
\000\000\000\000\046\000\000\000\000\000\000\000\000\000\000\000\
\000\000\046\000\046\000\046\000\046\000\046\000\046\000\073\000\
\000\000\046\000\046\000\000\000\000\000\061\000\062\000\063\000\
\064\000\000\000\000\000\065\000\066\000\067\000\068\000\069\000\
\070\000\096\000\000\000\071\000\072\000\000\000\000\000\061\000\
\062\000\063\000\064\000\000\000\000\000\065\000\066\000\067\000\
\068\000\069\000\070\000\098\000\000\000\071\000\072\000\000\000\
\000\000\061\000\062\000\063\000\064\000\000\000\000\000\065\000\
\066\000\067\000\068\000\069\000\070\000\000\000\000\000\071\000\
\072\000\061\000\062\000\063\000\064\000\000\000\000\000\065\000\
\066\000\067\000\068\000\069\000\070\000\000\000\000\000\071\000\
\072\000\061\000\062\000\063\000\064\000\000\000\000\000\065\000\
\066\000\067\000\068\000\069\000\070\000\000\000\000\000\071\000\
\061\000\062\000\063\000\064\000\000\000\000\000\065\000\066\000\
\067\000\068\000\069\000\070\000\061\000\062\000\063\000\064\000\
\000\000\000\000\000\000\000\000\067\000\068\000\069\000\070\000"

let yycheck = "\033\000\
\000\000\002\001\036\000\037\000\017\000\096\000\040\000\098\000\
\001\001\001\001\003\001\003\001\013\001\026\000\105\000\008\001\
\008\001\001\001\109\000\003\001\001\000\055\000\056\000\057\000\
\058\000\059\000\037\001\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\068\000\069\000\070\000\071\000\072\000\001\001\
\002\001\001\001\001\001\037\001\003\001\003\001\003\001\035\001\
\010\001\008\001\008\001\008\001\014\001\008\001\002\001\007\001\
\015\001\016\001\006\001\021\001\022\001\011\001\012\001\097\000\
\023\001\024\001\100\000\003\001\002\001\006\001\004\001\005\001\
\106\000\035\001\036\001\037\001\010\001\001\001\004\001\003\001\
\014\001\003\001\002\001\001\001\008\001\003\001\002\001\021\001\
\022\001\002\001\008\001\025\001\026\001\001\001\028\001\029\001\
\002\001\008\001\004\001\005\001\024\001\035\001\036\001\037\001\
\010\001\023\001\024\001\003\001\014\001\030\001\031\001\032\001\
\033\001\034\001\001\001\021\001\022\001\027\001\003\001\025\001\
\026\001\003\001\028\001\029\001\002\001\001\001\004\001\005\001\
\003\001\035\001\036\001\037\001\010\001\003\001\003\001\030\000\
\014\001\009\001\010\001\011\001\012\001\004\000\034\000\021\001\
\022\001\106\000\255\255\025\001\026\001\255\255\028\001\029\001\
\002\001\255\255\004\001\005\001\255\255\035\001\036\001\037\001\
\010\001\255\255\255\255\255\255\014\001\255\255\255\255\255\255\
\255\255\255\255\255\255\021\001\022\001\255\255\255\255\025\001\
\026\001\255\255\028\001\029\001\002\001\255\255\004\001\255\255\
\255\255\035\001\036\001\037\001\010\001\002\001\255\255\255\255\
\014\001\255\255\255\255\255\255\255\255\010\001\255\255\021\001\
\022\001\014\001\255\255\025\001\026\001\255\255\028\001\029\001\
\021\001\022\001\255\255\255\255\255\255\035\001\036\001\037\001\
\255\255\255\255\001\001\255\255\003\001\255\255\035\001\036\001\
\037\001\008\001\009\001\010\001\011\001\012\001\255\255\255\255\
\015\001\016\001\017\001\018\001\019\001\020\001\001\001\255\255\
\023\001\024\001\255\255\255\255\255\255\255\255\009\001\010\001\
\011\001\012\001\255\255\255\255\015\001\016\001\017\001\018\001\
\019\001\020\001\001\001\255\255\023\001\024\001\255\255\255\255\
\255\255\255\255\009\001\010\001\011\001\012\001\255\255\255\255\
\015\001\016\001\017\001\018\001\019\001\020\001\255\255\255\255\
\023\001\024\001\255\255\001\001\255\255\255\255\030\001\031\001\
\032\001\033\001\034\001\009\001\010\001\011\001\012\001\255\255\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\255\255\
\255\255\023\001\024\001\001\001\255\255\003\001\255\255\255\255\
\255\255\255\255\008\001\009\001\010\001\255\255\255\255\255\255\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\255\255\
\255\255\023\001\024\001\001\001\255\255\003\001\255\255\255\255\
\255\255\255\255\008\001\009\001\010\001\255\255\255\255\255\255\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\255\255\
\255\255\023\001\024\001\001\001\255\255\003\001\255\255\255\255\
\255\255\001\001\008\001\003\001\255\255\255\255\255\255\255\255\
\008\001\015\001\016\001\017\001\018\001\019\001\020\001\015\001\
\016\001\023\001\024\001\001\001\255\255\003\001\255\255\023\001\
\024\001\255\255\008\001\255\255\255\255\255\255\255\255\255\255\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\255\255\
\255\255\023\001\024\001\001\001\255\255\003\001\255\255\255\255\
\255\255\255\255\008\001\255\255\255\255\255\255\255\255\255\255\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\255\255\
\255\255\023\001\024\001\001\001\255\255\003\001\255\255\255\255\
\255\255\255\255\008\001\255\255\255\255\255\255\255\255\255\255\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\003\001\
\255\255\023\001\024\001\255\255\255\255\009\001\010\001\011\001\
\012\001\255\255\255\255\015\001\016\001\017\001\018\001\019\001\
\020\001\003\001\255\255\023\001\024\001\255\255\255\255\009\001\
\010\001\011\001\012\001\255\255\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\003\001\255\255\023\001\024\001\255\255\
\255\255\009\001\010\001\011\001\012\001\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\255\255\255\255\023\001\
\024\001\009\001\010\001\011\001\012\001\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\255\255\255\255\023\001\
\024\001\009\001\010\001\011\001\012\001\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\255\255\255\255\023\001\
\009\001\010\001\011\001\012\001\255\255\255\255\015\001\016\001\
\017\001\018\001\019\001\020\001\009\001\010\001\011\001\012\001\
\255\255\255\255\255\255\255\255\017\001\018\001\019\001\020\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  LBRACKET\000\
  RBRACKET\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  NOT\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  TRUE\000\
  FALSE\000\
  AND\000\
  OR\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  INT\000\
  BOOL\000\
  VOID\000\
  MAT\000\
  FLOAT\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  NUMBER\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 32 "src/parser.mly"
            ( _1 )
# 363 "src/parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "src/parser.mly"
                 ( [], [] )
# 369 "src/parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 36 "src/parser.mly"
               ( (_2 :: fst _1), snd _1 )
# 377 "src/parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 37 "src/parser.mly"
               ( fst _1, (_2 :: snd _1) )
# 385 "src/parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 41 "src/parser.mly"
     ( { typ = _1;
	 fname = _2;
	 formals = _4;
	 locals = List.rev _7;
	 body = List.rev _8 } )
# 400 "src/parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "src/parser.mly"
                  ( [] )
# 406 "src/parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 49 "src/parser.mly"
                  ( List.rev _1 )
# 413 "src/parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "src/parser.mly"
           ((_1, _2, 1))
# 421 "src/parser.ml"
               : 'bind))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 53 "src/parser.mly"
                                     ((_1, _2, _4))
# 430 "src/parser.ml"
               : 'bind))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bind) in
    Obj.repr(
# 56 "src/parser.mly"
                           ( [_1] )
# 437 "src/parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bind) in
    Obj.repr(
# 57 "src/parser.mly"
                           ( _3 :: _1 )
# 445 "src/parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "src/parser.mly"
        ( Int )
# 451 "src/parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "src/parser.mly"
         ( Bool )
# 457 "src/parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "src/parser.mly"
         ( Void )
# 463 "src/parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "src/parser.mly"
        ( Mat )
# 469 "src/parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "src/parser.mly"
          ( Float )
# 475 "src/parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "src/parser.mly"
                     ( [] )
# 481 "src/parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 68 "src/parser.mly"
                     ( _2 :: _1 )
# 489 "src/parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'bind) in
    Obj.repr(
# 71 "src/parser.mly"
             ( _1 )
# 496 "src/parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "src/parser.mly"
                   ( [] )
# 502 "src/parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 75 "src/parser.mly"
                   ( _2 :: _1 )
# 510 "src/parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 78 "src/parser.mly"
              ( Expr _1 )
# 517 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "src/parser.mly"
                ( Return Noexpr )
# 523 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 80 "src/parser.mly"
                     ( Return _2 )
# 530 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 81 "src/parser.mly"
                            ( Block(List.rev _2) )
# 537 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 82 "src/parser.mly"
                                            ( If(_3, _5, Block([])) )
# 545 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 83 "src/parser.mly"
                                            ( If(_3, _5, _7) )
# 554 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 85 "src/parser.mly"
     ( For(_3, _5, _7, _9) )
# 564 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 86 "src/parser.mly"
                                  ( While(_3, _5) )
# 572 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "src/parser.mly"
                  ( Noexpr )
# 578 "src/parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 90 "src/parser.mly"
                  ( _1 )
# 585 "src/parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 93 "src/parser.mly"
                     ( Literal(_1) )
# 592 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 94 "src/parser.mly"
                      ( Number(_1) )
# 599 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "src/parser.mly"
                     ( BoolLit(true) )
# 605 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "src/parser.mly"
                     ( BoolLit(false) )
# 611 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 97 "src/parser.mly"
                     ( Id(_1) )
# 618 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "src/parser.mly"
                     ( Binop(_1, Add,   _3) )
# 626 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "src/parser.mly"
                     ( Binop(_1, Sub,   _3) )
# 634 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "src/parser.mly"
                     ( Binop(_1, Mult,  _3) )
# 642 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "src/parser.mly"
                     ( Binop(_1, Div,   _3) )
# 650 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "src/parser.mly"
                     ( Binop(_1, Equal, _3) )
# 658 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "src/parser.mly"
                     ( Binop(_1, Neq,   _3) )
# 666 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "src/parser.mly"
                     ( Binop(_1, Less,  _3) )
# 674 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "src/parser.mly"
                     ( Binop(_1, Leq,   _3) )
# 682 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "src/parser.mly"
                     ( Binop(_1, Greater, _3) )
# 690 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "src/parser.mly"
                     ( Binop(_1, Geq,   _3) )
# 698 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "src/parser.mly"
                     ( Binop(_1, And,   _3) )
# 706 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "src/parser.mly"
                     ( Binop(_1, Or,    _3) )
# 714 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "src/parser.mly"
                         ( Unop(Neg, _2) )
# 721 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "src/parser.mly"
                     ( Unop(Not, _2) )
# 728 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "src/parser.mly"
                     ( Assign(_1, _3) )
# 736 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 113 "src/parser.mly"
                                 ( Call(_1, _3) )
# 744 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 114 "src/parser.mly"
                       ( _2 )
# 751 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "src/parser.mly"
                  ( [] )
# 757 "src/parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 118 "src/parser.mly"
                  ( List.rev _1 )
# 764 "src/parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "src/parser.mly"
                            ( [_1] )
# 771 "src/parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "src/parser.mly"
                            ( _3 :: _1 )
# 779 "src/parser.ml"
               : 'actuals_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
