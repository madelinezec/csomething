open Semantics

exception SEM_error of string 
exception NotFound

let rec string_of_type typ = 
    match typ with
    | Int -> "int"
    | Bool -> "bool"
    | Void -> "void"
    | Mat t -> "matrix of " ^ string_of_type t
    | Float -> "float"
    | Vec t -> "vector of " ^ string_of_type t 
    | Unknown -> "unknown type"

let wrap_exception a b = 
    try
        b ()
    with
        SEM_error ex ->
            raise (SEM_error (a ^ "\n" ^ ex))

let empty_dict = fun _ -> raise NotFound
let extend_dict name sym dict =
    fun n ->
        if String.equal n name then
          sym
        else
          dict n

let append_dict dict1 dict2 = 
    fun n ->
        try
            dict1 n
        with
            NotFound -> dict2 n

let in_dict name dict =
    try
        let _ = dict name in
            true
    with
        NotFound -> 
            false

        
let rec check_compat a b = 
    match a, b with
    |   (Int, Int) -> ()
    |   (Float, Int) -> ()
    |   (Int, Float) -> ()
    |   (Float, Float) -> ()
    |   (Bool, Bool) -> ()
    |  _ -> 
        let msg = "Expect " ^ (string_of_type a) ^ " but got " ^ (string_of_type b) in
            raise (SEM_error msg)
and is_compat a b = 
    try
        let _ = check_compat a b in 
            true
    with
        _ -> false
and check_ret_type ret typ = 
    match ret with
    | [] -> ()
    | r :: rest -> 
        check_compat typ r;
        check_ret_type ret typ


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
        



let rec check_decl st dict p = 
    match p with
    | VDecl {vtyp = typ; vname = id; vvalue = expr} ->
        if in_dict id dict then
            let msg = "Duplicate variable " ^ (string_of_type typ) ^ " " ^ id ^
                    ", previously defined as " ^ (show_decl (dict id)) in
                raise (SEM_error msg)
        else
            let result = 
                match !st#find id with
                | Some entry ->
                    let _ = check_expr (extend_dict id p dict) (Assign ((SymRefVar entry), expr))
                        in
                            extend_dict id p dict
                | None -> raise (SEM_error ("Can't find this symbol " ^ id))
            in
                result
    | FDecl {ftyp = typ; fname = id; formals = fms; fbody = (symtbl, stmtlist)} ->
        if in_dict id dict then
            let msg = "Duplicate function" ^ (string_of_type typ) ^ " " ^ id ^
                    ", previously defined as " ^ (show_decl (dict id)) in
                raise (SEM_error msg)
        else
            let formals = check_formals symtbl fms in
            let newdict = (extend_dict id p (append_dict formals dict)) in
            let _       = check_fdecl typ id symtbl stmtlist newdict in
               extend_dict id p dict
            
            

and check_formals st decls = 
    List.fold_left (check_decl st) empty_dict decls

and check_fdecl typ id symtbl stmtlist dict = 
    let (_, ret) = List.fold_left (check_stmt symtbl) (dict, []) stmtlist in 
    match typ with
        | Void -> 
            if all_void ret then
                typ
            else
                raise (SEM_error ("void function [" ^ id ^ "] returns non-void value"))
        | _ -> 
            if all_non_void ret then
                match List.find_opt (fun x -> not (is_compat typ x)) ret with
                | Some a ->
                    raise (SEM_error (string_of_type typ ^ " function [" ^ id ^ "] returns " ^ (string_of_type a)))
                | None -> 
                    if List.length ret == 0 then
                        raise (SEM_error (string_of_type typ ^ " function [" ^ id ^ "] must return a value"))
                    else
                        
                        if List.fold_left (||) false (List.map check_has_return stmtlist) then
                            typ
                        else
                            raise (SEM_error ("non-void function [" ^ id ^ "] does not always return a value"))
            else
                raise (SEM_error ("non-void function [" ^ id ^ "] returns void value"))


and check_has_return stmt = 
    match stmt with
    | Return expr  ->
        true
    | Block (_, list) ->
        List.fold_left (||) false (List.map check_has_return list)
    | If (_, t, f) ->
        check_has_return t && check_has_return f
    | While(cond, stmt) ->
        false
    | DeclStmt decl -> 
        false
    | Expr e ->
        false

and check_stmt symtbl (dict, ret) stmtlist =
    match stmtlist with
    | Return  expr  ->
        let typ = check_expr dict expr in
        (dict, List.cons typ ret)
    | Block (symtbl1, list) ->
        let (_, newret) = List.fold_left (check_stmt symtbl1) (dict, ret) list in 
            (dict, newret)
    | If (cond, t, f) ->
        let typ = check_expr dict cond in
            check_compat Bool typ;
            let (_, ret1) = check_stmt symtbl (dict, ret) t in 
            let (_, ret2) = check_stmt symtbl (dict, ret1) f in
            (dict, ret2)
    | While(cond, stmt) ->
        let typ = check_expr dict cond in
        check_compat Bool typ;
        check_stmt symtbl (dict,ret) stmt
    | DeclStmt decl -> 
        (check_decl symtbl dict (VDecl decl), ret)
    | Expr e ->
        let _ = check_expr dict e in
            (dict, ret)
                
and check_non_void typ =
    match typ with
        |   Void -> 
            let msg = "Can't be void" in
                raise (SEM_error msg)
        |  _ -> ()
    
and merge_type a b =
    match a, b with
    | None, None -> None
    | None, Some c -> Some c
    | Some c, None -> Some c
    | Some (Vec a), Some (Vec b) ->
        let result = 
            match merge_type (Some a) (Some b) with
            | Some c -> Some (Vec c) 
            | None -> None
        in result
    | Some c, Some d ->
        if c == d then
            Some c
        else
            raise (SEM_error ("Incompatible vector type " ^ (string_of_type c) ^ " and " ^ (string_of_type d)))

and get_vec_type list = 
    List.fold_left merge_type None list

and check_expr dict expr =
    match expr with
    | Literal _ -> Int
    | Number _  -> Float
    | BoolLit _ -> Bool
    | VecLit [] -> 
        Vec Void  (* any type is ok, will check in later assignment statement*)
    | VecLit list ->
        let result = 
            match get_vec_type (List.map (fun x -> Some (check_expr dict x)) list) with
            | None       -> Vec Void
            | Some Int   -> Vec Int
            | Some Float -> Vec Float
            | Some a -> raise (SEM_error ("Unsupported vector type " ^ (string_of_type a)))
        in result

    | MatLit list ->
        let collapsed = List.flatten list in
        let result = begin match get_vec_type (List.map (fun x -> Some (check_expr dict x)) collapsed) with
            | None -> Mat Void
            | Some Int -> Mat Int
            | Some Float -> Mat Float
            | Some a -> raise (SEM_error ("Unsupported mat type " ^ (string_of_type a)))
        end
        in result
    | Binop (l, op, r) ->
        let result = 
            match op with
            | Ast.Add
            | Ast.Sub
            | Ast.Mult
            | Ast.Div ->
                let lt = check_expr dict l in
                let rt = check_expr dict r in
                    check_compat Float lt;
                    check_compat Float rt;
                    let result = 
                        match lt, rt with 
                        | (Int, Int) -> Int
                        | _ -> Float
                    in result
            | Ast.Less
            | Ast.Leq
            | Ast.Greater
            | Ast.Geq -> 
                let lt = check_expr dict l in
                let rt = check_expr dict r in
                    check_compat Float lt;
                    check_compat Float rt;
                    Bool
            | Ast.And
            | Ast.Or ->
                let lt = check_expr dict l in
                let rt = check_expr dict r in
                    check_compat Bool lt;
                    check_compat Bool rt;
                    Bool
            | Ast.Equal
            | Ast.Neq ->
                let lt = check_expr dict l in
                let rt = check_expr dict r in
                    check_compat lt rt;
                    Bool
        in
        result
    | Unop (op, e) ->
        let result = 
            match op with
            | Ast.Neg ->
                let typ = check_expr dict e in  
                    check_compat Float typ;
                    typ
            | Ast.Not ->
                let typ = check_expr dict e in  
                    check_compat Bool typ;
                    Bool
        in
        result
    | SymRefVar (SymVar {sv_typ = typ}) ->
        typ
    | Assign (e1, e2) ->
        let result = 
            match e1 with
            | SymRefVar (SymVar sym)  -> 
                let l = check_expr dict e1 in
                let r = check_expr dict e2 in
                let result = 
                    match l, r with
                    |   (Mat Unknown, Mat b) ->
                            let _ = sym.sv_typ <- Mat b in 
                            Mat b
                    |   (Vec Unknown, Vec b) ->
                            let _ = sym.sv_typ <- Vec b in
                            Vec b
                    |   (Mat a, Mat b) ->
                            if a == b then
                                Mat a
                            else
                                raise (SEM_error ("Incompatible matrix type " ^ (string_of_type a) ^ " and " ^ (string_of_type b)))
                    |   (Vec a, Vec b) ->
                            if a == b then
                                Vec a
                            else
                                raise (SEM_error ("Incompatible vector type " ^ (string_of_type a) ^ " and " ^ (string_of_type b)))
                    | _ ->
                        let _ = check_compat l r in
                            l
                in
                result
            | SingleIndex _ ->
                let r = check_expr dict e2 in
                let _ = check_compat Float r in
                    r
            | DoubleIndex _ ->
                let r = check_expr dict e2 in
                let _ = check_compat Float r in
                    r
            | _ -> raise (SEM_error "Invalid LHS")
        in
        result
    | Call (SymFun {sf_rtyp = typ; sf_args = args; sf_name = id}, exprs) ->
            wrap_exception "In function call"
                (fun () ->
                    let actual = List.length exprs in 
                    let formal = List.length args in
                    if (actual != formal) then
                        raise (SEM_error 
                            ("function [" ^ id ^ "] has " ^ 
                             (string_of_int formal) ^ " argument(s), but got " ^
                             (string_of_int actual)))
                    else
                        let _ = List.map (fun (a, b) -> check_compat a (check_expr dict b) )
                                (List.combine args exprs) in
                        typ)
    | Noexpr ->
        Void
    | SingleIndex (e1, e2) ->
        let l = check_expr dict e1 in
        let r = check_expr dict e2 in
        let _ = check_compat Int r in
        let result = 
            match l with
            | Vec typ ->
                typ
            | _ -> 
                raise (SEM_error (string_of_type l ^ " can't be singly subscribed"))
        in 
        result
    | DoubleIndex (e1, e2, e3) ->
        let l = check_expr dict e1 in
        let r2 = check_expr dict e2 in
        let r3 = check_expr dict e3 in
        let _ = check_compat Int r2 in
        let _ = check_compat Int r3 in
        let result = 
            match l with
            | Mat typ ->
                typ
            | Vec (Vec typ) ->
                typ
            | _ ->
                raise (SEM_error (string_of_type l ^ " can't be doubly subscribed"))
        in 
        result
    | _ -> raise (SEM_error "Invalid AST")
        
let check p =
  match p with
  | Program (decls, st) -> 
        let _ = List.fold_left (check_decl st) empty_dict decls in
            p


    
