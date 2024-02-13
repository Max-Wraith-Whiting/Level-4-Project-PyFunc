(* Language Interpreter Runtime *)
open HM.Ast.Expr
open HM.Ast.Constant
open HM.Ast.OpBinary

module Env = struct
  type t = (string * tree) list
  
  let make = ([] : t)

  let get (env : t) key =
    let result = 
      try List.assoc key env with 
      Not_found -> raise (Errors.raise_error "Undefined variable look up!") in
    result

  let set (env : t) (key : string) (value : tree) = 
    let is_assigned = List.mem_assoc key env
    in (* Throws error if undefined *)
    if is_assigned then
      let env' = List.remove_assoc key env in
      (key, value) :: env'
    else 
      (key, value) :: env
end

module Interpreter = struct

  let rec eval (env : Env.t) = function
    | ExprConst c -> ExprConst c
    | ExprVar var -> eval_var env var
    | ExprOpBinary (op, expr_a, expr_b) -> eval_op_binary (env : Env.t) op expr_a expr_b
    | ExprIf (condition, expr_a, expr_b) -> eval_if (env : Env.t) condition expr_a expr_b
    | ExprLet (binder, value, expr) -> eval_let (env : Env.t) binder value expr
    | ExprLetRec (binder, value, expr) -> eval_letrec (env : Env.t) binder value expr
    | ExprApplic (func, arg) -> eval_applic (env : Env.t) arg func
    | ExprFunc (binder, body) -> ExprFunc(binder, body)
    | ExprPair (first, second) -> eval_pair (env : Env.t) first second
    | ExprLetPair (binder_a, binder_b, expr_a, expr_b) -> eval_let_pair (env : Env.t) binder_a binder_b expr_a expr_b
    | ExprFirst (ExprPair (first, _)) -> eval (env : Env.t) first
    | ExprSecond (ExprPair (_, second)) -> eval (env : Env.t) second
    | _ -> print_endline "Oh no! Invalid tree node!"; (ExprConst ConstUnit)

  and eval_op_binary (env : Env.t) op expr_a expr_b =
    let left = eval env expr_a in
    let right = eval env expr_b in
    match left, op, right with
    (* Integer operations *)
      | (ExprConst (ConstInt a)), Add,            (ExprConst (ConstInt b)) ->  ExprConst (ConstInt (a + b))
      | (ExprConst (ConstInt a)), Subtract,       (ExprConst (ConstInt b)) ->  ExprConst (ConstInt (a - b))
      | (ExprConst (ConstInt a)), Multiply,       (ExprConst (ConstInt b)) ->  ExprConst (ConstInt (a * b))
      | (ExprConst (ConstInt a)), Divide,         (ExprConst (ConstInt b)) ->  ExprConst (ConstInt (a / b))
    (* Boolean operations *)
      | (ExprConst (ConstBool a)), Less,          (ExprConst (ConstBool b)) -> ExprConst (ConstBool (a < b))
      | (ExprConst (ConstBool a)), Greater,       (ExprConst (ConstBool b)) -> ExprConst (ConstBool (a > b))
      | (ExprConst (ConstBool a)), LessEqual,     (ExprConst (ConstBool b)) -> ExprConst (ConstBool (a <= b))
      | (ExprConst (ConstBool a)), GreaterEqual,  (ExprConst (ConstBool b)) -> ExprConst (ConstBool (a >= b))
      | (ExprConst (ConstBool a)), Equal,         (ExprConst (ConstBool b)) -> ExprConst (ConstBool (a = b))
      | (ExprConst (ConstBool a)), NotEqual,      (ExprConst (ConstBool b)) -> ExprConst (ConstBool (a <> b))
      | (ExprConst (ConstBool a)), And,           (ExprConst (ConstBool b)) -> ExprConst (ConstBool (a && b))
      | (ExprConst (ConstBool a)), Or,            (ExprConst (ConstBool b)) -> ExprConst (ConstBool (a || b))
      | tree_a, op, tree_b -> print_endline ("Oh no! Invalid binary op!" ^ "\nLeft-side: " ^ (print_tree tree_a) ^ "\nOp: " ^ (pp op) ^ "\nRight-side: " ^ (print_tree tree_b)); ExprConst (ConstUnit)

  and eval_if (env : Env.t) condition expr_a expr_b =
    let cond = eval env condition in
    match cond with
      | ExprConst (ConstBool true) -> eval env expr_a
      | ExprConst (ConstBool false) -> eval env expr_b
      | ExprVar var -> eval_var env var
      | _ -> print_endline "Oh no! Invalid condition: Must be a boolean!"; ExprConst (ConstUnit)

  and eval_let (env : Env.t) binder let_expr in_expr =
    (* Compute value *)
    let value = eval env let_expr in
    (* Set (binder, value) in environment. *)
    let env' = Env.set env binder value in
    (* Eval expr with new environment. *)
    eval env' in_expr

  and eval_letrec (env : Env.t) binder let_expr in_expr =
    (* Set variable into the Env before evaluation. *)
    let env' = Env.set env binder let_expr in
    (* Compute value with recursive referencing. *)
    (* let value = eval env' value_node in *)
    eval env' in_expr

  and eval_var (env : Env.t) var =
    (* Attempt to get var from env *)
    let result = Env.get env var in
    result

  (* and eval_func (env : Env.env) binder body = *)
    (* You have an unbound variable as a parameter. *)
    (* Body is to be evaluated on the condition of application so NOT here. *)
    (* () *)

  and eval_applic (env : Env.t) arg (func : tree) =
    (* Func refers to either a function node or a variable node! *)
    let get_func_node (env : Env.t) = function
    | ExprFunc node -> ExprFunc node          (* If lambda is raw. *)
    | ExprVar var -> (eval_var env (var))     (* If lambda is called by via a variable. *)
    | x -> raise (Errors.Runtime_Error ("Called non-functional node:" ^ (get_name x)))
    in

    let unwrap = function 
    | ExprFunc (binder, body) -> (binder, body)
    | x -> raise (Errors.Runtime_Error ("ExprFunc unwrapping error! Node: " ^ get_name x))
    in

    let binder, body = unwrap (get_func_node env func) in 
    let env' = Env.set env binder arg in
      eval env' body


  and eval_pair (env : Env.t) first second =
    (* Evaluate the terms of the pair, but retain the structure. *)
    let value_first = eval env first in
    let value_second = eval env second in
      ExprPair (value_first, value_second)

  
  and eval_let_pair (env : Env.t) binder_a binder_b definition scope =
    let get_pair = function
      | ExprPair (first, second) -> (first, second)
      | x -> raise (Errors.Runtime_Error ("Let-Pair expression does not return a pair! Node: " ^ get_name x ))
    in
      
    (* Evaluate assinged expression. *)
    let pair_definition = eval env definition in
    (* Extract the pair values. *)
    let (value_a, value_b) = get_pair pair_definition in
    (* Set binder_a to value_a and binder_b to value_b. *)
    let env' = Env.set env binder_a value_a in
    let env'' = Env.set env' binder_b value_b in
      eval env'' scope

  let interpret (root_node : tree) =
    eval (Env.make) root_node
end