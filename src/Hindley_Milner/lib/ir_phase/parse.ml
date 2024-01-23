(* let main token lexbuf = failwith "undefined" *)

open Ir_lexer
open Lexing
open HM.Errors

let print_position ppf lexbuf = 
  let pos = lexbuf.lex_curr_p in 
  Format.fprintf ppf "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf = 
  try Ir_parser.expr_main Ir_lexer.token lexbuf with
  | Lexical_error msg -> 
    let msg = Format.asprintf "%a: %s" print_position lexbuf msg in raise (parse_error msg)
  | Ir_parser.Error ->
    let msg = Format.asprintf "%a: syntax error" print_position lexbuf in raise (parse_error msg)
  
let parse_string str = 
  let lexbuf = Lexing.from_string str in
    lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = "<string>"};
  let expr = parse_with_error lexbuf in expr

(* let parse_with_error lexbuf = try  *)