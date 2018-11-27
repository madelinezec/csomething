open Ast
exception AST_error of string 
type decl_type = VAR | FUN | NA [@@deriving show]
let empty_dict = fun _ -> (NA, Epsilon, [])
let extend_dict name value list dtype dict = 
    fun n ->
        if String.equal n  name then
          (dtype, value, list)
        else
          dict n

let append_dict dict1 dict2 = 
    fun n ->
        match (dict1 n) with
        |    (NA, Epsilon, _) -> dict2 n
        |    a -> a




let rec check_decls p dict = 
    match p with
    | DEmpty -> dict
    | Declaration(typ, id, Vdecl (rest)) ->
        let result = 
            match (dict id) with
            |   (_, Epsilon, _) -> check_decls rest (extend_dict id typ [] VAR dict)
            |   (a, t, _) ->
                let msg = "Duplicate variable " ^ (show_typ typ) ^ " " ^ id ^
                    ", previously defined as " ^ (show_decl_type a) ^ " " ^ (show_typ t) in
                    raise (AST_error msg) in
        result
    | Declaration(typ, id, Fdecl (fdecl, rest)) ->
        let result = 
            match (dict id) with
            |   (_, Epsilon, _) -> 
                let (locals, ret) = check_fdecl typ id fdecl dict in
                    check_decls rest (extend_dict id typ locals FUN dict)
            |   (a, t, _) ->
                let msg = "Duplicate function " ^ (show_typ typ) ^ " " ^ id ^
                    ", previously defined as " ^ (show_decl_type a) ^ " " ^ (show_typ t) in
                raise (AST_error msg) in
        result



and check_fo fo dict locals = 
    match fo with
    |   FormalsOptEmpty -> (dict, locals)
    |   FormalsOpt (FormalList (typ, id, rest)) -> 
            let result = 
            match (dict id) with
            |   (_, Epsilon, _) -> 
                    let res = 
                        let d = extend_dict id typ [] VAR dict in
                            match rest with
                            |   Fempty -> (d, List.concat [locals; [typ]])
                            |   FormalListPrime l -> 
                                    check_fo (FormalsOpt l) d (List.concat [locals; [typ]]) in
                        res
            |   (a, t, _) ->
                let msg = "Duplicate variable " ^ (show_typ typ) ^ " " ^ id ^
                    ", previously defined as " ^ (show_decl_type a) ^ " " ^ (show_typ t) in
                    raise (AST_error msg) in
            result

and check_fdecl typ id p dict = 
     match p with
     | Funcdecl (fo, _, stmtlist) ->
         let (localdict, locals) = check_fo fo empty_dict [] in
         let newdict = (extend_dict id typ locals FUN (append_dict dict localdict)) in
         let (_, ret) = check_stmt newdict stmtlist [] in 
            match typ with
            |   Void -> if all_void ret then
                           (locals, typ)
                        else
                            raise (AST_error "Void function returns value")
            |   _ -> 
                match ret with
                | [] ->  raise (AST_error "Non-void function must return a value");
                | _ -> let _ = check_ret_type ret typ in
                          (locals, typ)
and all_void list = 
        match list with
        |  [] -> true
        |  Void :: rest -> all_void rest
        |  _ :: rest -> false

and all_non_void list = 
        match list with
        |   [] -> true
        |   Void :: rest -> false
        |  _ :: rest -> all_non_void rest

and check_stmt dict stmtlist ret =
    match stmtlist with
    | StmtlistNil -> (dict, ret)
    | StmtList (Assignment asnmt, rest) -> 
        let (newdict, typ) = check_asnmt asnmt dict in
            check_stmt newdict rest (List.cons typ ret)
    | StmtList (Return  OptNil, rest)  ->
        check_stmt dict rest (List.cons Void ret)
    | StmtList (Return (StmtExpression asnmt), rest) ->
        let (newdict, typ) = check_asnmt asnmt dict in
            check_stmt newdict rest (List.cons typ ret)
    | StmtList (Parentheses list, rest) ->
        let (_, newret) = check_stmt dict list ret in
            check_stmt dict rest newret
    | StmtList (If (asnmt, stmt), rest) ->
        let (newdict, typ) = check_asnmt asnmt dict in
            check_compat Bool typ;
            check_stmt newdict (StmtList(stmt, StmtlistNil)) ret
    | StmtList (For(a1, a2, a3, stmt), rest) ->
        let (newdict,_) = check_asnmt a1 dict in
        let (n2, typ) = check_asnmt a2 newdict in
            check_compat Bool typ;
            let (n3, _) = check_asnmt a3 n2 in
                check_stmt n3 (StmtList(stmt, StmtlistNil)) ret
    | StmtList (While(asnmt, stmt), rest) ->
        let (newdict, typ) = check_asnmt asnmt dict in
        check_compat Bool typ;
        check_stmt newdict (StmtList(stmt, StmtlistNil)) ret
                
and check_compat a b = 
    match (a, b) with
    |   (Int, Int) -> ()
    |   (Bool, Bool) -> ()
    |  _ -> 
        let msg = "Expect " ^ (show_typ a) ^ " but got " ^ (show_typ b) in
            raise (AST_error msg)

and check_ret_type ret typ = 
    match ret with
    | [] -> ()
    | r :: rest -> 
        check_compat typ r;
        check_ret_type ret typ

and lookupvar var dict = 
    match (dict var) with
        |   (VAR, a, b) -> (VAR, a, b)
        |   _ ->
            let msg = "Undefined variable " ^ var in
                raise (AST_error msg)

and lookupfun var dict = 
    match (dict var) with
        |   (FUN, a, b) -> (FUN, a, b)
        |   _ ->
            let msg = "Undefined function " ^ var in
                raise (AST_error msg)
and check_non_void typ =
    match typ with
        |   Void -> 
            let msg = "Can't be void" in
                raise (AST_error msg)
        |  _ -> ()

and check_asnmt asnmt dict = 
    match asnmt with
    | NoAssign expr -> (dict, check_expr dict expr)
    | IDAssign (id, rhs) ->
        let result = 
            match rhs with
                | VariableAssign _ ->
                    let (decl, typ, _) = lookupvar id dict in
                    let (newdict, ret) = check_asnmt_id id dict rhs in
                    let _ = check_compat typ ret in
                        (newdict, ret)
                | MethodCall _ ->
                    check_asnmt_id id dict rhs
            in 
                result
    | TypeAssign (typ, id, rhs) ->
        let ret = check_expr dict rhs in 
        let _ = check_compat typ ret in
        let res = (extend_dict id typ [] VAR dict, ret) in 
            res


and check_asnmt_id id dict rhs =
    match rhs with
    |   MethodCall actuals ->  
            let (decl, typ, list) = lookupfun id dict in 
            let _ = check_compat_list dict list actuals in
                (dict, typ)
    |   VariableAssign expr ->
            (dict, check_expr dict expr)
    
and check_compat_list dict a b = 
    match (a, b) with
    | (a, ActualsList b) -> check_compat_list dict a b
    | ([], NoActuals) -> ()
    | (_, NoActuals) -> raise (AST_error "Actual arguments too few")
    | ([], _) -> raise (AST_error "Actual arguments too many")
    | (aa :: ra, SingleExpression (bb, rb)) ->
        check_compat aa (check_expr dict bb);
        check_compat_list dict ra rb
    

and check_expr dict expr =
    match expr with
    | Add (lhs, rhs) -> check_binary dict lhs rhs
    | Minus (lhs, rhs) -> check_binary dict lhs rhs
    | Times (lhs, rhs) -> check_binary dict lhs rhs
    | Divide (lhs, rhs) -> check_binary dict lhs rhs
    | IntLit _ -> Int
    | BoolLit _ -> Bool
    | Id a -> 
        let (_, typ, _) = lookupvar a dict in 
            typ

and check_binary dict lhs rhs =  
    let l = check_expr dict lhs in
        check_compat Int l;
        let r = check_expr dict rhs in
            check_compat Int r;
            Int

let check p =
  match p with
  | Program decls -> check_decls decls empty_dict
  | ProgramNil -> empty_dict


    
