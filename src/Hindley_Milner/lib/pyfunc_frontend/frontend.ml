open Py_ast.Expr
open Py_ast.OpBinary
open Py_ast.OpUnary

module Frontend = struct
  module Lexer = Py_lexer
  module Parser = Py_parser
  module IR = HM.Ast.Expr
  module BinOp = HM.Ast.OpBinary
  module UnOp = HM.Ast.OpUnary

  let pp_ast ast = Py_ast.Expr.print_tree ast

  let generate_ast source = 
    let open Parse in
    parse_string source

  exception Unimplemented of string
  let raise_unimpl msg = 
    raise (Unimplemented msg)
  
  exception BadConversion of string
  let raise_conversion msg = raise (BadConversion msg)

  let rec convert = function
    | Const c -> IR.ExprConst c
    | Var v -> IR.ExprVar v
    | If (cond, if_expr, else_expr) -> IR.ExprIf (convert cond, convert if_expr, convert else_expr)
    | OpBinary (op, expr_a, expr_b) -> convert_binary_op op expr_a expr_b
    | Program (bindings_list) -> convert_program bindings_list
    | List (value_list) -> convert_list value_list
    (* | Func (binder, param_list, body) -> convert_func binder param_list body *)
    | _ -> print_endline "Dunno"; raise_unimpl "Not implemented currently!"

  and convert_binary_op op expr_a expr_b =
    let left = convert expr_a in
    let right = convert expr_b in
    match op with
      | Add           -> ExprOpBinary (BinOp.Add, left, right)
      | Subtract      -> ExprOpBinary (BinOp.Subtract, left, right)
      | Multiply      -> ExprOpBinary (BinOp.Multiply, left, right)
      | Divide        -> ExprOpBinary (BinOp.Divide, left, right)
      | Less          -> ExprOpBinary (BinOp.Less, left, right)
      | Greater       -> ExprOpBinary (BinOp.Greater, left, right)
      | LessEqual     -> ExprOpBinary (BinOp.LessEqual, left, right)
      | GreaterEqual  -> ExprOpBinary (BinOp.GreaterEqual, left, right)
      | Equal         -> ExprOpBinary (BinOp.Equal, left, right)
      | NotEqual      -> ExprOpBinary (BinOp.NotEqual, left, right)
      | And           -> ExprOpBinary (BinOp.And, left, right)
      | Or            -> ExprOpBinary (BinOp.Or, left, right)
      | Cons          -> ExprOpBinary (BinOp.Cons, left, right)
  
  and convert_unary_op op expr = 
    let right = convert expr in
    match op with
      | Positive -> IR.ExprOpUnary (UnOp.Positive, right)
      | Negative -> IR.ExprOpUnary (UnOp.Negative, right)
      | Not      -> IR.ExprOpUnary (UnOp.Not, right)
  
  and convert_list value_list = 
    let converted_list = List.map (convert) (value_list) in
    ExprList converted_list
  
  and convert_global_bindings binding_list program_body = 
    let program = convert program_body in
    let blist = !binding_list in

    let split_func_def func_def =
      match func_def with
      | Func (binder, param_list, body) -> (binder, param_list, body)
      | _ -> raise (BadConversion "Non-func definition passed to global bindings converter!")
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

  and convert_program (binding_list : tree list) =

    let unwrap_binding = function
      | Binding (name, expr) -> (name, expr)
      | _ -> raise (BadConversion "Illegal object in program top-level.")
    in
  
    let filter_main blist =
          List.filter (fun x -> fst(unwrap_binding x) = "main") blist
    in

    (* Get the main expression of the program. *)
    let get_main blist =
      let filtered_list = filter_main blist in
      let is_single_main = if List.length filtered_list <> 1 then false else true in
      if is_single_main then
         let main_expr = snd (unwrap_binding (List.hd filtered_list)) in 
         main_expr
      else
        raise (BadConversion "A single main function must exist.")
    in

    (* Get all the bindings that are NOT main. *)
    let get_bindings blist = 
      List.filter (fun x -> fst(unwrap_binding x) <> "main") blist
    in

    let convert_binding (*binding : string*) (expr : tree) (scope : IR.tree) = 

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
      match expr with
        | Func (binder, param_list, tree) -> convert_func binder param_list tree scope
        | _ -> raise (Unimplemented "Currently only functions are supported as bindings.")
    in

    let rec convert_bindings (blist : tree list) (main_expr : IR.tree)  = 
      if List.is_empty blist then 
        main_expr
      else
        let _, expr = unwrap_binding (List.hd blist) in
        let new_expr = convert_binding expr main_expr in
        convert_bindings (List.tl blist) (new_expr)
    in

    let main_expr = convert (get_main binding_list) in
    convert_bindings (get_bindings binding_list) main_expr
end