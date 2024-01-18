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

open Ir_frontend
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

(* let rec interpret (*: HM.Ast.Constant.t *) =
  let open HM.Ast.Expr in
  let open HM.Ast.Constant in
  let open HM.Ast.OpBinary in
  (* let apply = () in *)
  (* let func = () in *)
  
  let binary_op op a_expr b_expr : tree = 
    let a = interpret a_expr in
    let b = interpret b_expr in
    let int_out x = ExprConst (ConstInt x) in
    let bool_out x = ExprConst (ConstBool x) in
    match op with
    | Add -> int_out (a + b)
    | Subtract -> int_out (a - b)
    | Multiply -> int_out (a * b)
    | Divide -> int_out (a / b)
    | Less -> bool_out (a < b)
    | Greater -> bool_out (a > b)
    | LessEqual -> bool_out (a <= b)
    | GreaterEqual -> bool_out (a >= b)
    | Equal -> bool_out (a == b)
    | NotEqual -> bool_out (a != b)
    | And -> bool_out (a && b)
    | Or -> bool_out (a || b)
  in
  
  let if_else cond_expr if_expr else_expr =
    let truth = interpret cond_expr in
    match truth with
      | true -> interpret if_expr
      | false -> interpret else_expr
  
  in

  (* let let_expr = () in *)
  (* let pair = () in  *)
  (* let let_pair = () in *)
  (* let first = () in *)
  (* let second = () in *)
  function
    | ExprOpBinary (op, a, b) -> binary_op op a b
    | ExprIf (cond, if_expr, else_expr) -> if_else cond if_expr else_expr
    | _ -> ExprConst (ConstBool false) *)