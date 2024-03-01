open Ast 
open Type
open OpBinary
open OpUnary
open Expr


module Typecheck = struct
  module ContextMap = Map.Make(String)
  module ContextSet = Set.Make(String)

  (* Stuff Dealing with Fresh Type variables *)
  let rec fresh_TVs = function
    | TypeVar typeVar -> ContextSet.singleton (TypeVar.var typeVar)
    | TypeFunc (type_a, type_b) -> ContextSet.union (fresh_TVs type_a) (fresh_TVs type_b)
    | TypePair (type_a, type_b) -> ContextSet.union (fresh_TVs type_a) (fresh_TVs type_b)
    | _ -> ContextSet.empty

  let fresh_monoTvs = fresh_TVs

  let fresh_polyTVs (quantifier, monotype) =
      ContextSet.diff (fresh_TVs monotype) quantifier

  (* Environment *)

  module Env = struct
    type t = ContextSet.t * (Type.polyType ContextMap.t)

    let empty = (ContextSet.empty, ContextMap.empty)

    let fresh_TVs = fst

    let bind_polytype k (quantifier, typ) (env_fresh_TVs, env) : t =
      let type_fresh_TVs = ContextSet.diff (fresh_monoTvs typ) (ContextSet.of_list quantifier) in
      (ContextSet.union env_fresh_TVs type_fresh_TVs,
       ContextMap.add k (quantifier, typ) env)
      
    let bind (k : Expr.variable) (typ : monoType) : t -> t =
      bind_polytype k ([], typ)

    let find k (_, env) = 
      match ContextMap.find_opt k env with
        | Some typ -> typ
        | None -> raise (Errors.type_error ("Unbound variable " ^ k))
  end
  type env = Env.t

  (* Environment End *)

  (* Instantiation:
     - Sub in fresh TVars for all given quantifiers. *)
  let instantiate poly_type =
    let (quant, mono) = poly_type in
    let substitution_map = List.map (fun q -> (q, TypeVar.fresh_TV ())) quant in
    let rec go = function
      | TypeVar type_var ->
        (
          match List.assoc_opt (TypeVar.var type_var) substitution_map with
            | Some instance -> TypeVar instance
            | None -> TypeVar type_var
        )
      | TypeFunc (type_a, type_b) -> TypeFunc (go type_a, go type_b)
      | TypePair (type_a, type_b) -> TypePair (go type_a, go type_b)
      | t -> t
    in
    go mono

  (* Instantiation End *)


  (* Generalisation:  *)
  let generalise env (monoType : Type.monoType) : Type.polyType =
    (List.map Quantifier.make 
      (ContextSet.elements 
        (ContextSet.diff (fresh_TVs monoType) (Env.fresh_TVs env))
      )
    ), monoType

  (* Unification *)

  exception Is_in_type

  let in_type_check type_var typ = 
    let rec go = function 
      | TypeVar tv -> if type_var = tv then raise Is_in_type
      | TypeFunc (type_a, type_b) | TypePair (type_a, type_b) -> go type_a; go type_b;
      | _ -> ()
    in
    try go typ with
      | Is_in_type -> 
        let error = Format.asprintf "[Is_in_type]: Check failed, %a appears in %a"
          TypeVar.pp type_var
          Type.pp typ
      in raise (Errors.Type_Error error)


  let rec is_value = function
    | ExprPair (expr_a, expr_b) -> is_value expr_a && is_value expr_b
    | ExprVar _ | ExprConst _ | ExprFunc _ -> true
    | _ -> false

  let rec unify type_a type_b = 
    match type_a, type_b with

      (* Matching types succeed by not throwing an error.*)
      | type_a, type_b when type_a = type_b -> () 
      
      (* Matching where types are pairs / funcs. *)
      | TypeFunc (a1, a2), TypeFunc (b1, b2) | TypePair (a1, a2), TypePair (b1, b2) -> 
        unify a1 b1; unify a2 b2
        
      (* Check type occurs, then unionfind to see if it is resolved or unresolved. *)
      | TypeVar type_var, t | t, TypeVar type_var -> 
        in_type_check type_var t;
        let res_state = TypeVar.resolution_state type_var in 
        (
          match UnionFind.get res_state with
          | Unresolved -> UnionFind.set res_state (Resolved t)
          | Resolved type_b -> unify type_b t
        )
      
      (* List Operation *)
      | TypeList a, TypeList b -> 
        unify a b

      | _, _ -> 
        let error = Format.asprintf "[Unification error]: Cannot unify %a with %a" 
          Type.pp type_a 
          Type.pp type_b 
        in 
        raise (Errors.type_error error)

  (* Unification End *)

  let typecheck_const = function
  | Constant.ConstString _ -> TypeString
  | Constant.ConstBool _ -> TypeBool
  | Constant.ConstInt _ -> TypeInt
  | Constant.ConstUnit -> TypeUnit
  
  (* Typecheck with a given context / environment. *)
  let rec _typecheck env = 

    (* Typechecking Functions *)
    let typecheck_func env binder body =
      let type_ann = new_var () in
      let env' = Env.bind binder type_ann env in
      let body_typ = _typecheck env' body in
      TypeFunc (type_ann, body_typ)
    in

    
    (* Typechecking Applications *)
    let typecheck_applic env expr_func expr_arg =
      let fresh_var = new_var () in
      let func_type = _typecheck env expr_func in
      let arg_type = _typecheck env expr_arg in
      unify func_type (TypeFunc (arg_type, fresh_var));
      fresh_var
    in
    
    (* Typechecking ifs *)
    let typecheck_if env expr_cond expr_if expr_else = 
      let cond_type = _typecheck env expr_cond in
      let if_type = _typecheck env expr_if in 
      let else_type = _typecheck env expr_else in
        unify cond_type TypeBool;
        unify if_type else_type;
        if_type
    in
    
    (* Typechecking Letrec:  letrec x = e in e' *)
    let typecheck_letrec env binder expr_a expr_b = 
      let env' = Env.bind binder (new_var ()) env in 
      let a_type = _typecheck env' expr_a in
      let env'' = Env.bind_polytype binder (generalise env' a_type) env' in
      let b_type = _typecheck env'' expr_b in
        b_type
    in

    (* Typechecking Lets *)
    let typecheck_let env binder expr_a expr_b = 
      let a_type = _typecheck env expr_a in
      let env' = 
        if is_value expr_a then
          Env.bind_polytype binder (generalise env a_type) env
        else
          Env.bind binder a_type env
      in 
      let b_type = _typecheck env' expr_b in
        b_type
    in

    let typecheck_let_pair env a b expr_a expr_b =
      let a_type = _typecheck env expr_a in
      let var_a = new_var () in
      let var_b = new_var () in
        unify (TypePair (var_a, var_b)) a_type;
        let env = Env.bind a var_a env in
        let env = Env.bind b var_b env in
        let b_type = _typecheck env expr_b in
          b_type
    in

    let typecheck_pair env expr_a expr_b =
      let a_type = _typecheck env expr_a in 
      let b_type = _typecheck env expr_b in
        TypePair (a_type, b_type)
    in

    let typecheck_first env expr =
      let typ = _typecheck env expr in
      TypePair (typ, new_var ())
    in
  
    let typecheck_second env expr =
      let typ = _typecheck env expr in
      TypePair (new_var (), typ)
    in

    (* Typechecking Binary Operations *)
    let typecheck_opbinary env op expr_a expr_b =
      let a_type = _typecheck env expr_a in
      let b_type = _typecheck env expr_b in
      match op with
        | And | Or ->
          unify a_type TypeBool;
          unify b_type TypeBool;
          TypeBool
        
        | Equal | NotEqual ->
          unify a_type b_type;
          TypeBool
        
        | Less | Greater| LessEqual | GreaterEqual ->
          unify a_type TypeInt;
          unify b_type TypeInt;
          TypeBool
          
        | Add | Multiply | Subtract | Divide ->
          unify a_type TypeInt;
          unify b_type TypeInt;
          TypeInt

        | Cons ->
          let fresh_var = new_var () in
          unify a_type fresh_var;
          unify b_type (TypeList fresh_var);
          b_type
    in

    let typecheck_list (env : Env.t) list = 
      let fresh_var = new_var () in
      
      (* Resolve types for all list elements. *)
      let resolved_list = List.map (_typecheck env) list in

      (* Unify types for all list elements. *)
      let () =
      try List.iter (fun x -> unify x fresh_var) resolved_list with
        | Errors.Type_Error msg -> print_endline ("Type Error - List type cannot be resolved: " ^ msg)
        | _ -> print_endline ("[typecheck_list] Something very bad has happened!")
      in
      (* Return ListType of head_type if unification successful. *)
      TypeList fresh_var
    in

    let typecheck_op_unary env op expr = 
      let typ = _typecheck env expr in
      match op with
        | Positive | Negative ->
          unify typ TypeInt;
          TypeInt;
        | Not ->
          unify typ TypeBool;
          TypeBool;
    in 

    
    (* Total Typechecker Function *)
    function
      | ExprVar value -> instantiate (Env.find value env)
      | ExprFunc (binder, body) -> typecheck_func env binder body
      | ExprApplic (expr_func, expr_arg) -> typecheck_applic env expr_func expr_arg
      | ExprOpUnary (op, expr) -> typecheck_op_unary env op expr
      | ExprOpBinary (op, expr_a, expr_b) -> typecheck_opbinary env op expr_a expr_b
      | ExprConst const -> typecheck_const const
      | ExprIf (expr_cond, expr_if, expr_else) -> typecheck_if env expr_cond expr_if expr_else
      | ExprLet (binder, expr_a, expr_b) -> typecheck_let env binder expr_a expr_b
      | ExprLetRec (binder, expr_a, expr_b) -> typecheck_letrec env binder expr_a expr_b
      | ExprLetPair (a, b, expr_a, expr_b) -> typecheck_let_pair env a b expr_a expr_b
      | ExprPair (expr_a, expr_b) -> typecheck_pair env expr_a expr_b
      | ExprFirst expr -> typecheck_first env expr
      | ExprSecond expr -> typecheck_second env expr
      | ExprList list -> typecheck_list (env : Env.t) list

  let rec resolve_types = function
    | TypeVar type_var ->
      (
        match UnionFind.get (TypeVar.resolution_state type_var) with
          | Unresolved -> TypeVar type_var
          | Resolved typ -> typ
      )
    | TypeFunc (type_a, type_b) -> TypeFunc (resolve_types type_a, resolve_types type_b)
    | TypePair (type_a, type_b) -> TypePair (resolve_types type_a, resolve_types type_b)
    | TypeList type_var -> TypeList (resolve_types type_var)
    | type_var -> type_var

  let typecheck expr =
    let typ = _typecheck Env.empty expr in
      resolve_types typ
end