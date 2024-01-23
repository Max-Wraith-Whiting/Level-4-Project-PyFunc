open Py_lexer
open Py_parser
open Lexing

exception Parse_Error of string

let parse_error str = Parse_Error str


let print_position ppf lexbuf = 
  let pos = lexbuf.lex_curr_p in 
    Format.fprintf ppf "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

(* [Internal] Parses a given string with error checking *)
let parse_with_error lexbuf = 
  try start token lexbuf with
  | Lexical_error msg -> 
    let msg = Format.asprintf "%a: %s" print_position lexbuf msg in raise (parse_error msg)

  | Error ->
    let msg = Format.asprintf "%a: syntax error" print_position lexbuf in raise (parse_error msg)

(* Parses a given string with the Pyfunc-frontend *)
let parse_string str = 
  let lexbuf = Lexing.from_string str in
    lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = "<string>"};
    let expr = parse_with_error lexbuf in expr

(* Generates a Unix-like tree of the AST*)
let get_ast str =
  Py_ast.Expr.print_tree (parse_string str)