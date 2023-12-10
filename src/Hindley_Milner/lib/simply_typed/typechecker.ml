open Types
open Type
open OpBinary
open Types.Expr


module Typecheck = struct
  module Context = Map.Make (String)
  type context = Type.t Context.t

  
  let typecheck_const = function
  | Constant.ConstString _ -> TypeString
  | Constant.ConstBool _ -> TypeBool
  | Constant.ConstInt _ -> TypeInt
  | Constant.ConstUnit -> TypeUnit
  
  (* Typecheck with a given context / environment. *)
  let rec _typecheck context = 

    (* Typechecking Functions *)
    let typecheck_func context binder typ body = 
      let newContext = Context.add binder typ context in
      let body_type = _typecheck newContext body in
        TypeFunc (typ, body_type) 
    in

    (* Typechecking Applications *)
    let typecheck_applic context expr_func expr_arg =
      let type_func = _typecheck context expr_func in
      let type_arg = _typecheck context expr_arg in
      match type_func with
        | TypeFunc (type_input, type_output) ->
          if type_arg <> type_input then
            raise (Errors.type_error "Argument type mismatch: Hahahha, I haven't implemented this yet.")
          else type_output
        | _ ->
          raise (Errors.type_error "Cannot call non-function.")
    in

    (* Typechecking ifs *)
    let typecheck_if context expr_cond expr_if expr_else = 
      let type_cond = _typecheck context expr_cond in 
      let type_if = _typecheck context expr_if in 
      let type_else = _typecheck context expr_else in 
        if (type_cond <> TypeBool) then 
          raise (Errors.type_error "The condition of an 'if' must be type bool.")
        else 
          if (type_if <> type_else) then 
            raise (Errors.type_error "Both branches of a conditional must have the same type.")
          else type_if
    in
    
    (* Typechecking Annotations *)
    let typecheck_ann context expr ann =
      let typ = _typecheck context expr in 
        if (typ <> ann) then
          raise (Errors.type_error "Type of expression didn't match annotation.")
        else typ
    in
    
    (* Typechecking Lets *)
    let typecheck_let context binder expr_a expr_b = 
      let type_a = _typecheck context expr_a in
      let newContext = Context.add binder type_a context in
        _typecheck newContext expr_b
    in
  
    (* Typechecking Binary Operations *)
    let typecheck_opbinary context op expr_a expr_b =
      let type_a = _typecheck context expr_a in
      let type_b = _typecheck context expr_b in
      match op with 
        | And | Or -> 
          if (type_a <> TypeBool || type_b <> TypeBool) then 
            raise (Errors.type_error "Arguments for && or || must are not boolean.")
          else TypeBool

        | Equal | NotEqual ->
          if (type_a <> type_b) then 
            raise (Errors.type_error "Arguments for == or != are not the same type.") 
          else TypeBool

        | Less | Greater | LessEqual | GreaterEqual ->
          if (type_a <> TypeInt || type_b <> TypeInt) then 
            raise (Errors.type_error "Arguments for Int comparison must be Int.") 
          else TypeBool

        | Add | Multiply | Subtract | Divide ->
          if (type_a <> TypeInt || type_b <> TypeInt) then 
            raise (Errors.type_error "Arguments for arithmetic must be Int.") 
          else TypeInt
    in
    
    (* Total Typechecker Function *)
    function
    | ExprVar value -> Context.find value context
    | ExprFunc (binder, typ, body) -> typecheck_func context binder typ body
    | ExprApplic (expr_func, expr_arg) -> typecheck_applic context expr_func expr_arg
    | ExprOpBinary (op, expr_a, expr_b) -> typecheck_opbinary context op expr_a expr_b
    | ExprConst const -> typecheck_const const
    | ExprLet (binder, expr_a, expr_b) -> typecheck_let context binder expr_a expr_b
    | ExprAnn (expr, ann) -> typecheck_ann context expr ann
    | ExprIf (expr_cond, expr_if, expr_else) -> typecheck_if context expr_cond expr_if expr_else


  let typecheck expr = _typecheck Context.empty expr
end 