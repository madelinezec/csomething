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


    method debug_dump : unit =
        let print_one name sym =
            print_endline (name ^ ": " ^ printer sym) in
        Hashtbl.iter print_one htable;
        print_endline "-------------"
end
