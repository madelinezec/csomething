(* token *)
type token = Main | End
           | Identifier of string
           | Read | Write
           | Literal of int
           | Assign
           | LeftParen | RightParen
           | AddOp | SubOp
           | Comma | Semicolon

type scanner = { mutable last_token: token option; stm: stream }

exception Syntax_error of string

let syntax_error s msg = raise (Syntax_error (msg ^ " on line " ^ (string_of_int s.stm.line_num)))


(* skip all blank and new line characters *)
let rec skip_blank_chars stm = let c = read_char stm in
                               if c = ' ' || c = '\t' || c = '\r' || c = '\n'
                               then skip_blank_chars stm
                               else unread_char stm c; ()

(* scan a stream and return next token *)
let scan s =   let stm = s.stm in let c = read_char stm in
               let rec scan_iden acc = let nc = read_char stm in
                                       if is_alpha nc || is_digit nc || nc='_'
                                       then scan_iden (acc ^ (Char.escaped nc))
                                       else let _ = unread_char stm nc in
                                            let lc = String.lowercase acc in
                                            if lc = "main" then Main
                                            else if lc = "end" then End
                                            else if lc = "print" then Print
                                            else if lc = "read" then Read
                                            else if lc = "write" then Write
                                            else Identifier acc
               in
               let rec scan_lit acc = let nc = read_char stm in
                                      if is_digit nc
                                      then scan_lit (acc ^ (Char.escaped nc))
                                      else let _ = unread_char stm nc in
                                           Literal (int_of_string acc)
               in
               if is_alpha c then scan_iden (Char.escaped c)
               else if is_digit c then scan_lit (Char.escaped c)
               else if c='+' then AddOp
               else if c='-' then SubOp
               else if c=',' then Comma
               else if c=';' then Semicolon
               else if c='(' then LeftParen
               else if c=')' then RightParen
               else if c=':' && read_char stm = '=' then Assign
               else syntax_error s "couldn't identify the token"

let new_scanner stm = { last_token=None; stm=stm }

let match_next s = match s.last_token with
                      None -> let _ = skip_blank_chars s.stm in scan s
                    | Some tn -> s.last_token <- None; tn

let match_token s t = match_next s = t

let next_token s = match s.last_token with
                        None ->  (skip_blank_chars s.stm;
                                  let t = scan s in
                                  s.last_token <- Some t; t)
                      | Some t -> t
