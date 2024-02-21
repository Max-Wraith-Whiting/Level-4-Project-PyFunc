open Py_ast.Expr

module Frontend = struct
  module Lexer = Py_lexer
  module Parser = Py_parser
  module IR = HM.Ast.Expr
  module Op = HM.Ast.OpBinary

  let pp_ast ast = Py_ast.Expr.print_tree ast

  let generate_ast source = 
    let open Parse in
    parse_string source

  exception Unimplemented of string
  let raise_unimpl msg = 
    raise (Unimplemented msg)

  let rec convert = function
    | Const c -> IR.ExprConst c
    | Var v -> IR.ExprVar v
    | If (cond, if_expr, else_expr) -> IR.ExprIf (convert cond, convert if_expr, convert else_expr)
    | OpBinary (op, expr_a, expr_b) -> convert_binary_op op expr_a expr_b
    (* | Func (binder, param_list, body) -> convert_func binder param_list body *)
    | _ -> raise_unimpl "Not implemented currently!"

  and convert_binary_op op expr_a expr_b =
    let left = convert expr_a in
    let right = convert expr_b in
    match op with
      | Add           -> ExprOpBinary (Op.Add, left, right)
      | Subtract      -> ExprOpBinary (Op.Subtract, left, right)
      | Multiply      -> ExprOpBinary (Op.Multiply, left, right)
      | Divide        -> ExprOpBinary (Op.Divide, left, right)
      | Less          -> ExprOpBinary (Op.Less, left, right)
      | Greater       -> ExprOpBinary (Op.Greater, left, right)
      | LessEqual     -> ExprOpBinary (Op.LessEqual, left, right)
      | GreaterEqual  -> ExprOpBinary (Op.GreaterEqual, left, right)
      | Equal         -> ExprOpBinary (Op.Equal, left, right)
      | NotEqual      -> ExprOpBinary (Op.NotEqual, left, right)
      | And           -> ExprOpBinary (Op.And, left, right)
      | Or            -> ExprOpBinary (Op.Or, left, right)
  (* and convert_func binder param_list body =
    let p_list = List.rev param_list in
    let rec make_lambdas p_list (body : IR.tree) =
      let inner = IR.ExprFunc (List.hd p_list, body) in
      make_lambdas (List.tl p_list) inner
    in
    let lambdas = make_lambdas p_list (convert body) in
      IR.ExprLetRec (binder, lambdas,  *)
end