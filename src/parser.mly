%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE INT BOOL VOID MAT VEC FLOAT
%token <int> LITERAL
%token <float> NUMBER
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
    | /* nothing */ { [] }
    | decl decls { $1 :: $2 }

decl:
    | vdecl SEMI { VDecl $1 }
    | fdecl { FDecl $1 }


arraylit:
         LBRACE exprlist RBRACE{VecLit $2}

exprlist:
         expr{[$1]}
        |expr COMMA exprlist{$1::$3}
        
fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
     { { ftyp = $1;
	 fname = $2;
	 formals = $4;
	 fbody = $7 } }
    | typ ID LPAREN formals_opt RPAREN SEMI
        {
            {ftyp = $1; fname = $2; formals = $4; fbody = []}
        }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    vdecl                   { [$1] }
  | formal_list COMMA vdecl { $3 :: $1 }

typ:
    INT { Int }
  | BOOL { Bool }
  | VOID { Void }
  | MAT { Mat }
  | VEC { Vec}
  | FLOAT { Float }
  | typ LBRACKET LITERAL RBRACKET { RealVec($1, $3) }
  | typ LBRACKET LITERAL COMMA LITERAL RBRACKET { RealMat($1, $3, $5) }

vdecl_list:
    /* nothing */    { [] }
  | vdecl SEMI vdecl_list { $1 :: $3 }

vdecl:
    | typ ID { {vtyp = $1;  vname = $2; vvalue = None} }
    | typ ID ASSIGN expr { {vtyp = $1; vname = $2; vvalue = Some $4} }

stmt_list:
    /* nothing */  { [] }
  | stmt stmt_list { $1 :: $2 }

stmt:
    expr SEMI { Expr $1 }
  | vdecl SEMI { DeclStmt $1 } 
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block($2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1) }
  | NUMBER            { Number($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | ID               { Id($1) }
  | arraylit         { $1 } 
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | NOT expr         { Unop(Not, $2) }
  | expr ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }
  | expr LBRACKET expr RBRACKET { SingleIndex($1,$3)}
  | expr LBRACKET expr RBRACKET LBRACKET expr RBRACKET { DoubleIndex($1,$3,$6)}
actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
