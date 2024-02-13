(* Language Interpreter Runtime *)
open HM.Ast.Expr
open HM.Ast.Constant
open HM.Ast.OpBinary

type value = 
  | Vint of int 
  | Vbool of bool 
  | Vstring of string 
  | Vunit of unit 
  | Vvar of string 
  | Vtree of tree 
  | Vref of value ref
  | Vpair of (value * value)

let rec pp_value = function
  | Vint i -> string_of_int i
  | Vbool b -> string_of_bool b
  | Vstring s -> s
  | Vunit _ -> "()"
  | Vvar var -> var
  | Vtree _ -> "!ast node!"
  | Vpair (a, b) -> (pp_value a ^ "*" ^ pp_value b)
  | Vref v -> pp_value (!v)

module Env = struct
  type entry = EntryVar of value | EntryTree of tree
  type t = (string * entry) list
  
  let make = ([] : t)

  let get (env : t) key =
    let result = 
      try List.assoc key env with 
      Not_found -> raise (Errors.raise_lookup_error "Undefined variable look up!") in
    result

  let set (env : t) (key : string) (value : entry) = 
    let is_assigned = List.mem_assoc key env
    in (* Throws error if undefined *)
    if is_assigned then
      (* Probably need to make this an error. Lack of immutability is dangerous. *)
      let env' = List.remove_assoc key env in
      (key, value) :: env'
    else 
      (key, value) :: env
end

module Interpreter = struct

  let eval_const = function
    | ConstInt i -> Vint i
    | ConstBool b -> Vbool b
    | ConstString s -> Vstring s
    | ConstUnit -> Vunit ()
    (* | _ -> raise (Errors.Runtime_Error "Eval error: Invalid Const evaluation!") *)

  let rec eval (env : Env.t) = 
    (* print_endline "eval"; *)
    function
    | ExprConst c -> eval_const c
    | ExprVar var -> eval_var env var
    | ExprOpBinary (op, expr_a, expr_b) -> eval_op_binary (env : Env.t) op expr_a expr_b
    | ExprIf (condition, expr_a, expr_b) -> eval_if (env : Env.t) condition expr_a expr_b
    | ExprLet (binder, value, expr) -> eval_let (env : Env.t) binder value expr
    | ExprLetRec (binder, value, expr) -> eval_letrec (env : Env.t) binder value expr
    | ExprApplic (func, arg) -> eval_applic (env : Env.t) arg func
    | ExprFunc (binder, body) -> print_endline "eval_func"; Vtree (ExprFunc(binder, body))
    | ExprPair (first, second) -> eval_pair (env : Env.t) first second
    | ExprLetPair (binder_a, binder_b, expr_a, expr_b) -> eval_let_pair (env : Env.t) binder_a binder_b expr_a expr_b
    | ExprFirst (ExprPair (first, _)) -> eval (env : Env.t) first
    | ExprSecond (ExprPair (_, second)) -> eval (env : Env.t) second
    | _ -> print_endline "Oh no! Invalid tree node!"; (Vunit ())

  and eval_op_binary (env : Env.t) op expr_a expr_b =
    (* print_endline "eval_op_binary"; *)
    let left = eval env expr_a in
    let right = eval env expr_b in
    match left, op, right with
    (* Integer operations *)
      | (Vint a), Add,      (Vint b) ->  Vint (a + b)
      | (Vint a), Subtract, (Vint b) ->  Vint (a - b)
      | (Vint a), Multiply, (Vint b) ->  Vint (a * b)
      | (Vint a), Divide,   (Vint b) ->  Vint (a / b)
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
      | _, op, _ -> print_endline ("Oh no! Invalid binary op: " ^ pp op); Vunit ()

  and eval_if (env : Env.t) condition expr_a expr_b =
    (* print_endline "eval_if"; *)
    let cond = eval env condition in
    match cond with
      | Vbool true -> eval env expr_a
      | Vbool false -> eval env expr_b
      | Vvar var -> eval_var env var
      | _ -> raise (Errors.Runtime_Error "Oh no! Invalid condition: Must be a boolean!")

  and eval_let (env : Env.t) binder let_expr in_expr =
    (* print_endline "eval_let"; *)
    (* Compute value *)
    let value = eval env let_expr in
    (* Set (binder, value) in environment. *)
    let env' = Env.set env binder (EntryVar value) in
    (* Eval expr with new environment. *)
    eval env' in_expr

  and eval_letrec (env : Env.t) binder let_expr in_expr =
    (* print_endline "eval_letrec"; *)
    (* Set variable into the Env before evaluation. *)
    let env' = Env.set env binder (EntryTree let_expr) in
    (* Compute value with recursive referencing. *)
    (* let value = eval env' value_node in *)
    eval env' in_expr

  and eval_var (env : Env.t) var =
    (* print_endline "eval_var"; *)
    (* Attempt to get var from env *)
    let result = Env.get env var in
    match result with
      | EntryVar v -> v        (* Return the value from scope. *)
      | EntryTree t -> Vtree t (* Return the tree node wrapped as a value. *)

  and eval_applic (env : Env.t) arg (func : tree) =
    (* print_endline "eval_applic"; *)
    (* Func refers to either a function node or a variable node! *)
    let unwrap_vtree = function
      | Vtree t -> t
      | _ -> raise (Errors.Runtime_Error "Non-Vtree value presented!")
    in
    let get_func_node (env : Env.t) = function
    | ExprFunc node -> ExprFunc node                 (* If lambda is raw. *)
    | ExprVar var -> unwrap_vtree (eval_var env var) (* If lambda is called by via a variable. *)
    | x -> raise (Errors.Runtime_Error ("Called non-functional node:" ^ (get_name x)))
    in
    let get_body_and_binder = function
      | ExprFunc (body, binder) -> (body, binder)
      | _ -> raise (Errors.Runtime_Error ("Attempted non-lambda node unwrap!")) 
    in
    let binder, body = get_body_and_binder (get_func_node env func) in 
    let arg_value = eval env arg in
    let env' = Env.set env binder (EntryVar arg_value) in
      eval env' body


  and eval_pair (env : Env.t) first second =
    (* print_endline "eval_pair"; *)
    (* Evaluate the terms of the pair, but retain the structure. *)
    let value_first = eval env first in
    let value_second = eval env second in
      Vpair (value_first, value_second)

  
  and eval_let_pair (env : Env.t) binder_a binder_b definition scope =   
    (* print_endline "eval_let_pair"; *)
    (* Evaluate assinged expression. *)
    let pair_definition = 
    match eval env definition with
      | Vpair (a, b) -> (a, b)
      | _ -> raise (Errors.Runtime_Error ("Let-Pair definition does not return a pair value!"))
    in
    (* Extract the pair values. *)
    let (value_a, value_b) = pair_definition in
    (* Set binder_a to value_a and binder_b to value_b. *)
    let env' = Env.set env binder_a (EntryVar value_a) in
    let env'' = Env.set env' binder_b (EntryVar value_b) in
      eval env'' scope

  let interpret (root_node : tree) =
    eval (Env.make) root_node
end