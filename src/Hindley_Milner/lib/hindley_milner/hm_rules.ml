open Types
open Types.Expr
open Type

module Hm_rules = struct
  module ContextMap = Map.Make(String)
  module ContextSet = Set.Make(String)

  (* Stuff ealing with Fresh Type variables *)
  let rec fresh_TVs = function
    | TypeVar typeVar -> ContextSet.singleton (TypeVar.var typeVar)
    | TypeFunc (type_a, type_b) -> ContextSet.union (fresh_TVs type_a) (fresh_TVs type_b)
    | TypePair (type_a, type_b) -> ContextSet.union (fresh_TVs type_a) (fresh_TVs type_b)
    | _ -> ContextSet.empty

    let fresh_polyTVs (quantifier, monotype) =
      ContextSet.diff (fresh_TVs monotype) quantifier

  (* Environment *)

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
  let generalise env monoType =
    (List.map Type.Quantifier.make 
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
    | ExprAnn (e, _) -> is_value e
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
      
      | _, _ -> 
        let error = Format.asprintf "[Unification error]: Cannot unify %a with %a" pp type_a pp type_b in 
        raise (Errors.type_error error)

    (* Unification End *)
end