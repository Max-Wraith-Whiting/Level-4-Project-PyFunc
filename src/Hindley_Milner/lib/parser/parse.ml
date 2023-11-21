(* let main token lexbuf = failwith "undefined" *)

open Lexer
open Lexing

let parse_with_error lexbuf = 

let parse_string str = 
  let lexbuf = Lexing.from_string str in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = "<string>"};
  let expr = parse_with_error lexbuf in 
    expr

(* let parse_with_error lexbuf = try  *)