open Lambda_sig
open Lexing

module Make(Lambda: LAMBDA) = struct
  module LambdaParser = Parser.Make(Lambda)

  let parse_string s () = 
    let lexbuf = Lexing.from_string s in
    lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = "<string>"};
    let expr = LambdaParser.expr_main Lexer.token lexbuf in
    expr
end