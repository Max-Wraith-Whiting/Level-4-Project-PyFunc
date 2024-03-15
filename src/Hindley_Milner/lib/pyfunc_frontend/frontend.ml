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

  (* let preprocess source =
    let get_indent str = 
      let rec count_space_helper idx = 
        if idx >= String.length str || str.[idx] <> ' ' then
          idx
        else
          count_space_helper (idx + 1)
      in
      let indent = count_space_helper 0 in
      if (indent mod 4 = 0) then
          indent / 4
      else
        raise (Parse.parse_error "Invalid indentation! 4 space indentation required.")
    in
    let rec loop prev_indent = function
      | [] -> 
        if prev_indent > 0 then
          print_string (String.make prev_indent '}')
        else 
          ()
      | line :: rest ->
        let indent = get_indent line in
        let diff = abs (prev_indent - indent) in
        if indent > prev_indent then
          print_string (String.make diff '{' ^ line)
        else if indent < prev_indent then
          print_string (line ^ String.make diff '}')
        else
          print_string line;
        print_endline "";
        loop indent rest
    in
    loop 0 source *)

  let preprocess source =
    let get_indent str = 
      let rec count_space_helper idx = 
        if idx >= String.length str || str.[idx] <> ' ' then
          idx
        else
          count_space_helper (idx + 1)
      in
      let indent = count_space_helper 0 in
      if (indent mod 4 = 0) then
        indent / 4
      else
        raise (Parse.parse_error "Invalid indentation! 4 space indentation required.")
    in
    let rec loop prev_indent acc = function
      | [] -> 
        if prev_indent > 0 then
          let closing_braces = String.make prev_indent '}' in
          List.rev (closing_braces :: acc)
        else 
          List.rev acc
      | line :: rest ->
        let indent = get_indent line in
        let diff = abs (prev_indent - indent) in
        let amended_line =
          if indent > prev_indent then
            (String.make diff '{') ^ line
          else if indent < prev_indent then
          (String.make diff '}') ^ line
          else
            line
        in
        loop indent (amended_line :: acc) rest
    in
    let output = String.concat "\n" (loop 0 [] source) in
    output
    
  let generate_ast source = 
    let open Parse in
    let processed_source = preprocess source in
    parse_string processed_source

  exception Unimplemented of string
  let raise_unimpl msg = 
    raise (Unimplemented msg)
  
  exception BadConversion of string
  let raise_conversion msg = raise (BadConversion msg)

  let rec convert = function
    | Program (bindings_list) -> convert_program bindings_list
    (* | Binding (binder, expr) ->  *)
    | Call (binder, arg_list) -> convert_call binder arg_list
    | Var v -> IR.ExprVar v
    | Const c -> IR.ExprConst c
    | If (cond, if_expr, else_expr) -> IR.ExprIf (convert cond, convert if_expr, convert else_expr)
    | OpBinary (op, expr_a, expr_b) -> convert_binary_op op expr_a expr_b
    | OpUnary (op, expr) -> convert_unary_op op expr
    | List (value_list) -> convert_list value_list
    (* | Func (binder, param_list, body) -> convert_func binder param_list body *)
    | x -> raise_unimpl ("[" ^ get_name x ^ "] Not implemented currently!")

  (* and convert_call binder arg_list = *)
    (* 1. Assume the function is already defined. *)
    (* 2. Generate  *)

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
      | Mod           -> ExprOpBinary (BinOp.Mod, left, right)
  
  and convert_unary_op op expr = 
    let right = convert expr in
    match op with
      | Positive -> IR.ExprOpUnary (UnOp.Positive, right)
      | Negative -> IR.ExprOpUnary (UnOp.Negative, right)
      | Not      -> IR.ExprOpUnary (UnOp.Not, right)
  
  and convert_list value_list = 
    let converted_list = List.map (convert) (value_list) in
    ExprList converted_list
  
  and convert_call binder arg_list = 
    (* Lookup binder *)
    (* If binder is function then: *)
      (* Create applic nodes for each given arg in the correct order. *)
      let processed_list = List.map (convert) arg_list in
      let function_node = IR.ExprVar (binder) in
      let rec call_to_applic a_list (body : IR.tree) = 
        if List.is_empty a_list then 
          body
        else
          let call_body = IR.ExprApplic (body, List.hd a_list) in
          call_to_applic (List.tl a_list) call_body
      in
      call_to_applic processed_list function_node

    (* Else: Throw cannot call binder! *)

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

    let rec convert_bindings (blist : tree list) (main_expr : IR.tree)  = 
      if List.is_empty blist then 
        main_expr
      else
        let _, expr = unwrap_binding (List.hd blist) in
        let new_expr = convert_binding expr main_expr in
        convert_bindings (List.tl blist) (new_expr)
    in

    let split_func_def func_def =
      match func_def with
      | Func (binder, param_list, body) -> (binder, param_list, body)
      | _ -> raise (BadConversion "Non-func definition passed to global bindings converter!")
    in
    let main_ast = (get_main binding_list) in

    let main_binder, main_param_list, main_body = split_func_def main_ast in
    let main_expr = convert_func main_binder main_param_list main_body (IR.ExprVar ("main")) in

    convert_bindings (get_bindings binding_list) main_expr 
end