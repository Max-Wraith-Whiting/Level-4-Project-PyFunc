(* Interpreter for running the AST *)

(*
  Language Constructs:
  1. Lambda
  2. If Else (will need provision for implicit empty else)
  3. Binary ops
  4. application of lambdas
  5. Let expressions

  Interpreter processing stack.
*)

open Pyfunc_frontend
(* open Lexing *)
(* open HM.Ast.Expr *)

(* Print AST *)
(* let ast_error str =
  let ast = parse_string str in
  pp ast *)
  (* try Parser.expr_main Lexer.token lexbuf with
    | Lexical_error msg ->
      let msg = Format.asprintf "%a: %s" print_position lexbuf msg in raise (parse_error msg)

    | Parser.Error ->
      let msg = Format.asprintf "%a: syntax error" print_position lexbuf in raise (parse_error msg)
*)

let ast_parse str = 
  let lexbuf = Lexing.from_string str in
  let ast = Parser.expr_main Lexer.token lexbuf in
  ast