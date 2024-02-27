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
  
  exception Conversion of string
  let raise_conversion msg = raise (Conversion msg)

  let rec convert = function
    | Const c -> IR.ExprConst c
    | Var v -> IR.ExprVar v
    | If (cond, if_expr, else_expr) -> IR.ExprIf (convert cond, convert if_expr, convert else_expr)
    | OpBinary (op, expr_a, expr_b) -> convert_binary_op op expr_a expr_b
    | Program (binding_list, program_body) -> convert_global_bindings binding_list program_body
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

  and convert_global_bindings binding_list program_body = 
    let program = convert program_body in
    let blist = !binding_list in

    let split_func_def func_def =
      match func_def with
      | Func (binder, param_list, body) -> (binder, param_list, body)
      | _ -> raise (Conversion "Non-func definition passed to global bindings converter!")
    in

    let convert_func func_binder func_param_list func_body scope =
      let p_list = List.rev func_param_list in

      let rec func_to_lambdas p_list (body : IR.tree) =
        if List.is_empty p_list then
          body
        else
          let lambda_body = IR.ExprFunc (List.hd p_list, body) in
          func_to_lambdas (List.tl p_list) lambda_body
      in
      let lambdas = func_to_lambdas p_list (convert func_body) in
        IR.ExprLetRec (func_binder, lambdas, scope)
    in
    let rec convert_bindings binding_list (program : IR.tree) = 
      if List.is_empty binding_list then
        program
      else
        let name, params, body = split_func_def (List.hd binding_list) in
        let func_instance = convert_func name params body program in
        convert_bindings (List.tl binding_list ) func_instance
      in
      let result = convert_bindings blist program in
      binding_list := []; (* This dictates that the binding list ref is cleared!*)
      result 

end