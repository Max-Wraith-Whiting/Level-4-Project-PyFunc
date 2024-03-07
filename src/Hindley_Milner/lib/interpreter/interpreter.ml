(* Language Interpreter Runtime *)
open HM.Ast.Expr
open HM.Ast.Constant
open HM.Ast.OpBinary
open HM.Ast.OpUnary

module Errors = struct
  (* Basic runtime error management *)
  exception Runtime_Error of string
  exception Lookup_Error of string

  let runtime_error msg = Runtime_Error msg
  let lookup_error msg = Lookup_Error msg
end

type value = 
  | Vint of int 
  | Vbool of bool 
  | Vstring of string 
  | Vunit of unit 
  | Vvar of string 
  | Vtree of (tree) 
  | Vref of value ref
  | Vpair of (value * value)
  | Vlist of value list

let rec pp_value = function
  | Vint i -> string_of_int i
  | Vbool b -> string_of_bool b
  | Vstring s -> "\"" ^ s ^ "\""
  | Vunit _ -> "()"
  | Vvar var -> var
  | Vtree t -> get_name t  (* Considered an illegal output! *)
  | Vpair (a, b) -> (pp_value a ^ "*" ^ pp_value b)
  | Vref v -> pp_value (!v)
  | Vlist l -> 
    let rec join seperator = function
      | [] -> ""
      | [x] -> x
      | ""::xs -> join seperator xs
      | x::xs -> x ^ seperator ^ (join seperator xs)
    in
    "[" ^ (join ", " (List.map pp_value l)) ^ "]"



module Env = struct
  type entry = EntryVar of value | EntryTree of tree
  type t = (string * entry) list
  
  let make = ([] : t)

  let get (env : t) key =
    let result = 
      try List.assoc key env with 
      Not_found -> raise (Errors.lookup_error ("Undefined variable '" ^ key ^ "' !")) in
    result

  let set (env : t) (key : string) (value : entry) = 
    let is_assigned = List.mem_assoc key env
    in (* Throws error if undefined *)
    if is_assigned then
      (* Probably need to make this an error. Mutability is dangerous. *)
      let env' = List.remove_assoc key env in
      (key, value) :: env'
    else 
      (key, value) :: env

  let remove (env : t) (key : string) = 
    let is_assigned = List.mem_assoc key env in
    if is_assigned then
      let env' = List.remove_assoc key env in
      env'
    else
      env
end



module Interpreter = struct

  let ref_env = ref Env.make

  let clear_ref_env = 
    ref_env := Env.make

  let set_ref_env env =
    ref_env := env

  let eval_const = function
    | ConstInt i -> Vint i
    | ConstBool b -> Vbool b
    | ConstString s -> Vstring s
    | ConstUnit -> Vunit ()
    (* | _ -> raise (Errors.Runtime_Error "Eval error: Invalid Const evaluation!") *)

  let rec eval = 
    (* print_endline "eval"; *)
    function
    | ExprConst c -> eval_const c
    | ExprVar var -> eval_var var
    | ExprOpUnary (op, expr) -> eval_op_unary op expr
    | ExprOpBinary (op, expr_a, expr_b) -> eval_op_binary op expr_a expr_b
    | ExprIf (condition, expr_a, expr_b) -> eval_if condition expr_a expr_b
    | ExprLet (binder, value, expr) -> eval_let binder value expr
    | ExprLetRec (binder, value, expr) -> eval_letrec binder value expr
    | ExprApplic (func, arg) -> eval_applic arg func
    | ExprFunc (binder, body) -> Vtree (ExprFunc(binder, body)) (* First-order functions can be values. *)
    | ExprPair (first, second) -> eval_pair first second
    | ExprLetPair (binder_a, binder_b, expr_a, expr_b) -> eval_let_pair binder_a binder_b expr_a expr_b
    | ExprFirst (ExprPair (first, _)) -> eval first
    | ExprSecond (ExprPair (_, second)) -> eval second
    | ExprList list -> eval_list list
    | _ -> print_endline "Oh no! Invalid tree node!"; (Vunit ())

  and eval_op_unary op expr = 
    let expr_value = eval expr in
    match op, expr_value with
      | Positive, (Vint i) -> Vint (~+i)
      | Negative, (Vint i) -> Vint (~-i)
      | Not, (Vbool b) -> Vbool (not b)
      | _, _ -> raise (Errors.runtime_error ("Oh no! Invalid unary op: " ^ op_unary_pp op))

  and eval_op_binary op expr_a expr_b =
    (* print_endline "eval_op_binary"; *)
    let left = eval expr_a in
    let right = eval expr_b in

    let modulo x y = 
      let remainder = x mod y in
        if remainder >= 0 then 
          remainder
        else
          remainder + y
      in

    match left, op, right with
    (* Integer operations *)
      | (Vint a), Add,      (Vint b) ->  Vint (a + b)
      | (Vint a), Subtract, (Vint b) ->  Vint (a - b)
      | (Vint a), Multiply, (Vint b) ->  Vint (a * b)
      | (Vint a), Divide,   (Vint b) ->  Vint (a / b)
      | (Vint a), Mod,      (Vint b) ->  Vint (modulo a b)
    (* Comparitive Int operations *)
      | (Vint a), Less,         (Vint b) -> Vbool (a < b)
      | (Vint a), Greater,      (Vint b) -> Vbool (a > b)
      | (Vint a), LessEqual,    (Vint b) -> Vbool (a <= b)
      | (Vint a), GreaterEqual, (Vint b) -> Vbool (a >= b)
      | (Vint a), Equal,        (Vint b) -> Vbool (a = b)
      | (Vint a), NotEqual,     (Vint b) -> Vbool (a <> b)
    (* Comparitive Boolean Operations *)
      | (Vbool a), Equal,    (Vbool b) -> Vbool (a = b)
      | (Vbool a), NotEqual, (Vbool b) -> Vbool (a <> b)
    (* Logical operations *)
      | (Vbool a), And, (Vbool b) -> Vbool (a && b)
      | (Vbool a), Or,  (Vbool b) -> Vbool (a || b)
    (* List Operations *)
      | (a), Cons, (Vlist b) -> Vlist (a :: b)
      | _, op, _ -> raise (Errors.runtime_error ("Oh no! Invalid binary op: " ^ op_binary_pp op))

  and eval_if condition expr_a expr_b =
    let cond = eval condition in
    match cond with
      | Vbool true -> eval expr_a
      | Vbool false -> eval expr_b
      | Vvar var -> eval_var var
      | _ -> raise (Errors.runtime_error "Oh no! Invalid condition: Must be a boolean!")

  and eval_let binder let_expr in_expr =
    let env = !ref_env in
    (* Compute value *)
    let value = eval let_expr in
    (* Set (binder, value) in environment. *)
    let env' = Env.set env binder (EntryVar value) in
    (* Eval expr with new environment. *)
    ref_env := env';
    eval in_expr

  and eval_letrec binder let_expr in_expr =
    let env = !ref_env in
    (* Set variable into the Env before evaluation. *)
    let env' = Env.set env binder (EntryTree let_expr) in
    (* Compute value with recursive referencing. *)
    (* let value = eval value_node in *)
    ref_env := env';
    eval in_expr

  and eval_var var =
    let env = !ref_env in
    (* Attempt to get var from env *)
    try let result = Env.get env var in
      match result with
        | EntryVar v -> v             (* Return the value from scope. *)
        | EntryTree t -> (eval t) (* Return the evaluated tree node. *)

    with
      | Errors.Lookup_Error msg -> raise (Errors.Lookup_Error msg)
      | _ -> raise (Errors.runtime_error "Oh shit.")  

  and eval_applic arg (unchecked_node : tree) =
     
    let func_lookup (env : Env.t) var = 
      let entry = Env.get env var in 
      match entry with
        | EntryTree tree -> tree
        | EntryVar l -> 
          match l with 
          | Vtree t -> t 
          | _ -> raise (Errors.runtime_error "Oh fuck!")
    in

    let get_node = function
      | Vtree t -> t
      | v -> raise (Errors.runtime_error ("Cannot call '" ^ pp_value v ^ "'!"))
    in
    
    let rec unwrap_tree =
    let env = !ref_env in
    function
      | ExprFunc (binder, body) -> (binder, body)
      | ExprVar (var) -> unwrap_tree (func_lookup env var)
      | _ -> raise (Errors.runtime_error "Invalid node passed to (eval_applic) unwrap_tree!");
    in
    (* let applic_tree = HM.Ast.Expr.print_tree (get_node (eval env unchecked_node)) in *)
    let arg_value = eval arg in
    let binder, func_body = unwrap_tree (get_node (eval unchecked_node)) in
    let env = !ref_env in
    let env' = Env.set env binder (EntryVar arg_value) in
      ref_env := env';
      eval func_body


  and eval_pair first second =
    (* print_endline "eval_pair"; *)
    (* Evaluate the terms of the pair, but retain the structure. *)
    let value_first = eval first in
    let value_second = eval second in
      Vpair (value_first, value_second)

  
  and eval_let_pair binder_a binder_b definition scope =  
    let env = !ref_env in 
    let pair_definition = 
    match eval definition with
      | Vpair (a, b) -> (a, b)
      | _ -> raise (Errors.runtime_error ("Let-Pair definition does not return a pair value!"))
    in
    (* Extract the pair values. *)
    let (value_a, value_b) = pair_definition in
    (* Set binder_a to value_a and binder_b to value_b. *)
    let env' = Env.set env binder_a (EntryVar value_a) in
    let env'' = Env.set env' binder_b (EntryVar value_b) in
      ref_env := env'';
      eval scope


  and eval_list list =
    let value_list = List.map (eval) list in
    Vlist value_list

  let interpret (root_node : tree) =
    eval root_node
end