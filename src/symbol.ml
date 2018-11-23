class ['t] symbol_table (parent : 't symbol_table ref option) printer =
object (self)
    val _parent = parent
    val htable : (string, 't) Hashtbl.t = Hashtbl.create 10

    method add (name : string) (s : 't)  =
        Hashtbl.add htable name s

    (* recursively look for symbol along the parent chain *)
    method find (name : string) : 't option =
        let this_res = Hashtbl.find_opt htable name in
        match this_res with
            | Some s -> Some s
            | None ->
                match _parent with
                    | Some r -> !r#find name
                    | None -> None


    method debug_dump : string =
        let ref_list = ref [] in
        let print_one name sym =
            ref_list := (name ^ ": " ^ printer sym) :: !ref_list in
        let _ = Hashtbl.iter print_one htable in
        String.concat "\n" (List.rev !ref_list)
end

let pp_symbol_table _ _ ht = print_endline ht#debug_dump
