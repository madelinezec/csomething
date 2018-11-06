let parseProgram toklis = let(r, parseTree) = parseDecl (tl toklis)
			in (if hd r = EOF then (SOME KIND OF AST THING)
					  else raise SyntaxError)
and parse_decl toklis = 
	let identity toklis =
		match toklis.lookahead with
			Lexer.ID -> (next toklis, Ast.ID??)
	in 
	let(toklis, decl_type) = parse_type toklis in
	let(toklis, decl_id) = identity toklis in 
	let(toklis, decl_prime) = parse_decl_prime toklis 
	in
	(toklis, Ast.decl(decl_type, decl_id, decl_prime))

and parse_decl_prime toklis = 
			if toklis.lookahead = Lexer.Semi then 
				let(toklis, vdecl) = parse_decl next toklis
				Ast.Semi???
			else 
			match toklis.lookahead with 
			| Lexer.Semi -> (next toklis, Ast.Semi)
			| Lexer.Lapren -> 
				let(toklis, e) = parse_fdecl toklis
				







and parse_type toklis = match toklis.lookahead with 
			| INT i -> (tl toklis, *some ast function*)
			| BOOL b -> (tl toklis, *some ast function*)
			| VOID v -> (tl toklis, *some ast function*)
			| _ -> raise Syntax Error


