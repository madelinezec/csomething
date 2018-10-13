open Stream

type token =
    | ID of string
    | Numeral of string
    | Float of float
    | Int
    | Matrix
    | LeftParens
    | RightParens
    | LeftBrace
    | RightBrace
    | LeftBracket
    | RightBracket
    | Semicolon
    | If
    | Else
    | While
    | Do
    | For
    | Switch
    | Case
    | Break
    | Continue
    | EOF

(* character *)
let is_digit c = let code = Char.code c in
                 code >= Char.code('0') && code <= Char.code('9')

let is_alpha c = let code = Char.code c in
                 (code >= Char.code('A') && code <= Char.code('Z')) ||
                 (code >= Char.code('a') && code <= Char.code('z'))
           

let rec read_until_special_char st = let c = peek st in
    match c with
        Some c -> 
            if not (is_alpha c) && (not (is_digit c)) then
                ""
            else
                String.make 1 c ^ read_until_special_char st
      | None -> ""

(* assuming the next token is an identifier, get it *)
let scan_id st = ID (read_until_special_char st) 

(* assuming the next token is a numeral, get it *)

let scan_numeral st = Numeral (read_until_special_char st)

(* scan the stream and get the next token *)
let scan st = let c = peek st in
    match c with 
        None -> EOF
      | Some c ->
            if is_alpha c then scan_id st
            else if is_digit c then scan_numeral st
            else EOF
