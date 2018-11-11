open Stream

type token =
    | ID of string
    | Numeral of string
    | Int
    | Float
    | Void
    | Bool
    | Matrix
    | LeftParens
    | RightParens
    | LeftBrace
    | RightBrace
    | LeftBracket
    | RightBracket
    | Plus
    | Not
    | Increment
    | Decrement
    | Minus
    | Multiply
    | Divide
    | Mod
    | Greater
    | Less
    | Equality
    | Neq
    | Assignment 
    | Leq
    | Geq
    | Semicolon
    | Comma
    | If
    | Else
    | While
    | Do
    | For
    | Switch
    | Case
    | Break
    | Continue
    | Return
    | EOF
    | True
    | False
    | And
    | Or
    | Invalid
    [@@deriving show]

exception ImpossibleChar of char

(* character *)
let is_digit c = let code = Char.code c in
                 code >= Char.code('0') && code <= Char.code('9')

let is_alpha c = let code = Char.code c in
                 (code >= Char.code('A') && code <= Char.code('Z')) ||
                 (code >= Char.code('a') && code <= Char.code('z'))

let is_space c = c = ' ' || c = '\t' || c = '\r' || c = '\n'

(* read until the next is not a letter or number *)
let rec read_until_special_char st = let c = peek st in
    match c with
        Some c -> 
            if not (is_alpha c) && (not (is_digit c)) then
                ""
            else
                (ignore @@ next st; String.make 1 c ^ read_until_special_char st)
      | None -> ""

let scan_keyword_or_id st =
    let str = read_until_special_char st in
    match str with
        "int" -> Int
      | "void" -> Void
      | "bool" -> Bool
      | "mat" -> Matrix
      | "if" -> If
      | "else" -> Else
      | "while" -> While
      | "do" -> Do
      | "for" -> For
      | "switch" -> Switch
      | "case" -> Case
      | "break" -> Break
      | "continue" -> Continue
      | "float" -> Float
      | "return" -> Return
      | "true" -> True
      | "false" -> False
      | s -> ID s

(* assuming the next token is a numeral, get it *)
let scan_numeral st = Numeral (read_until_special_char st)

(* scan the stream and get the next token *)
let rec scan st =
    let c = peek st in
    match c with 
        None -> EOF
      | Some c ->
            if is_space c then (ignore @@ next st; scan st)
            else if is_alpha c then scan_keyword_or_id st
            else if is_digit c then scan_numeral st
            else match next st with
                '(' -> LeftParens
              | ')' -> RightParens
              | '[' -> LeftBracket
              | ']' -> RightBracket
              | '{' -> LeftBrace
              | '}' -> RightBrace
              | '+' ->
                    begin match peek st with
                        Some '+' -> ignore (next st); Increment
                      | _ -> Plus
                    end
              | '-' ->
                    begin match peek st with
                        Some '+' -> ignore (next st); Decrement
                      | _ -> Minus
                    end
              | '*' -> Multiply
              | '/' -> Divide
              | '%' -> Mod
              | '=' ->
                    begin match peek st with
                        Some '=' -> ignore (next st); Equality
                      | _ -> Assignment
                    end
              | '!' -> 
                    begin match peek st with
                        Some '=' -> ignore (next st); Neq
                      | _ -> Not
                    end
               
              | '>' ->
                    begin match peek st with
                        Some '=' -> ignore (next st); Geq
                      | _ -> Greater
                    end
              | '<' ->
                    begin match peek st with
                        Some '=' -> ignore (next st); Leq
                      | _ -> Less
                    end
              | '&' ->
                    begin match peek st with
                        Some '&' -> ignore (next st); And
                      | _ -> Invalid
                    end
              | '|' ->
                    begin match peek st with
                        Some '|' -> ignore (next st); Or
                      | _ -> Invalid
                    end
              | ',' -> Comma
              | ';' -> Semicolon
              | x -> raise @@ ImpossibleChar x


(* returns all tokens from a stream *)
let rec tokenize st =
    let token = scan st in
    match token with
        EOF -> [EOF]
      | token -> token :: tokenize st

(* tokenize a file *)
let tokenize_file filename =
    let st = Stream.of_channel @@ open_in filename in
    tokenize st

