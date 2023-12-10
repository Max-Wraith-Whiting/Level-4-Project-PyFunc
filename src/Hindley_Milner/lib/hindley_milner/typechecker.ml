open Types
open Type
open OpBinary
open Types.Expr


module Typecheck = struct
  module ContextMap = Map.Make (String)
  module ContextSet = Set.Make(String)

  (* Stuff ealing with Fresh Type variables *)
  let rec fresh_TVs = function
    | TypeVar typeVar -> ContextSet.singleton (TypeVar.var typeVar)
    | TypeFunc (type_a, type_b) -> ContextSet.union (fresh_TVs type_a) (fresh_TVs type_b)
    | TypePair (type_a, type_b) -> ContextSet.union (fresh_TVs type_a) (fresh_TVs type_b)
    | _ -> ContextSet.empty

    let fresh_polyTVs (quantifier, monotype) =
      ContextSet.diff (fresh_TVs monotype) quantifier

  (* Environment definition and management goes here. *)

  module Env = struct
    type t = ContextSet.t * (Type.polyType ContextMap.t)

    let empty = (ContextSet.empty, ContextMap.empty)

    let fresh_TVs = fst

    
    let bind_polytype k (quantifier, typ) (env_fresh_TVs, env) =
      let type_fresh_TVs = ContextSet.diff (fresh_TVs typ) (ContextSet.of_list quantifier) in
      (ContextSet.union env_fresh_TVs type_fresh_TVs,
      ContextMap.add k (quantifier, typ) env)
      
    let bind k typ = bind_polytype k ([], typ)

    let find k (_, env) = 
      match ContextMap.find_opt k env with
        | Some typ -> typ
        | None -> raise (Errors.type_error ("Unbound variable " ^ k))
  end
  type env = Env.t

  (* Instantiation:
     - Sub in fresh TVars for all given quantifiers. *)

  (* Generalisation:  *)
  let generalise env monoType =
    (List.map Type.Quantifier.make 
      (ContextSet.elements 
        (ContextSet.diff (fresh_TVs monoType) (Env.fresh_TVs env)) (*Here goes (Some var lookup in the ContextSet) and (Context look up in env)*) 
      )
    ), monoType

  (* Unification:  *)

  let rec unify type_a type_b = 
    match type_a, type_b with

      | type_a, type_b when type_a = type_b -> () (* Matching types succeed by not throwing an error.*)
      
      | TypeFunc (a1, a2), TypeFunc (b1, b2) | TypePair (a1, a2), TypePair (b1, b2) -> (* Matching where types are pairs / funcs. *)
        unify a1 b1; unify a2 b2
        
      | TypeVar type_v, t | t, TypeVar type_v -> () (* Check type occurs, then let ufind point Union find to esolved or unresolved. *)
      
      | _, _ -> 
        (* let error = Format.asprintf "Cannot unify %a with %a" (pp type_a) (pp type_b) in  *)
        raise (Errors.type_error "[STUB]: Type error")


      (* Shoehorn TypeVars in here *)
      (* | _, _ -> raise Errors.type_error Format.asprintf "Cannot unify t0 with t1"  *)

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
    | ExprVar value -> instantiate (Env.find v env)
    | ExprFunc (binder, opt_ann, body) -> typecheck_func context binder opt_ann body
    
    | ExprApplic (expr_func, expr_arg) -> typecheck_applic context expr_func expr_arg
    | ExprOpBinary (op, expr_a, expr_b) -> typecheck_opbinary context op expr_a expr_b
    | ExprConst const -> typecheck_const const
    | ExprLet (binder, expr_a, expr_b) -> typecheck_let context binder expr_a expr_b
    | ExprAnn (expr, ann) -> typecheck_ann context expr ann
    | ExprIf (expr_cond, expr_if, expr_else) -> typecheck_if context expr_cond expr_if expr_else
    | ExprLetPair (x, y, expr_a, expr_b) -> typecheck_let_pair x y expr_a expr_b
    | ExprPair (expr_a, expr_b) -> typecheck_pair expr_a expr_b
    | ExprFirst expr -> typecheck_first expr
    | ExprSecond expr -> typecheck_second expr

  let typecheck expr = _typecheck Context.empty expr
end