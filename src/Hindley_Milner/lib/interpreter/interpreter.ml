(* Interpreter for running the AST *)
open HM.Ast.Expr
open HM.Ast.Constant
open HM.Ast.OpBinary
(* open Errors *)

module Interpreter = struct
  
  type env = (string * tree) list

  let get (env: env) key = 
    let result = List.assoc key env in function
      | Not_found -> print_string "Oh no! Not in env"; None
      | _ -> Some result

  let set (env: env) (key : string) (value : tree) =
    let result = (key, value) :: env in
      result

  let is_truthy value =
    match value with
      | ConstBool b -> b
      | ConstUnit -> false
      | _ -> true

  let is_equal left right =
    match left, right with
      | ConstUnit, ConstUnit -> true
      | ConstUnit, _ -> false
      | ConstBool l, ConstBool r -> l = r
      | _, _ -> false

  type value = Vint of int | Vstring of string | Vbool of bool | Vunit of unit
  
  (* Const *)
  let eval_const = function 
    | ConstBool b -> Vbool b
    | ConstInt i -> Vint i
    | ConstString s -> Vstring s
    | ConstUnit -> Vunit ()

  (* General Interpreter *)
  let rec eval (env : env)  = function
    (* | ExprVar v -> eval_var v env *)
    | ExprOpBinary (op, expr_a, expr_b) -> eval_op_binary op expr_a expr_b env
    | ExprConst c -> eval_const c
    | _ -> print_string "Oh no! Invalid tree node"; Vunit ()
  

  (* Var *)
  and eval_var = ()

  (* Let *)
  and eval_let = () 

  (* Let Rec *)
  and eval_letrec = ()

  (* Op Binary *)
  and eval_op_binary op expr_a expr_b env =
    let left = eval env expr_a in
    let right = eval env expr_b in
    match left, op, right with
    (* Integer Operations *)
    | (Vint a), Add, (Vint b) -> Vint (a + b)
    | (Vint a), Subtract, (Vint b) -> Vint (a + b)
    | (Vint a), Multiply, (Vint b) -> Vint (a + b)
    | (Vint a), Divide, (Vint b) -> Vint (a + b)
    (* Boolean Operations *)
    | (Vbool a), Less, (Vbool b) -> Vbool (a < b)
    | (Vbool a), Greater, (Vbool b) -> Vbool (a > b)
    | (Vbool a), LessEqual, (Vbool b) -> Vbool (a <= b)
    | (Vbool a), GreaterEqual, (Vbool b) -> Vbool (a >= b)
    | (Vbool a), Equal, (Vbool b) -> Vbool (a = b)
    | (Vbool a), NotEqual, (Vbool b) -> Vbool (a <> b)
    | (Vbool a), And, (Vbool b) -> Vbool (a && b)
    | (Vbool a), Or, (Vbool b) -> Vbool (a || b)
    | _, _, _ -> print_string "Oh no! Invalid binary op!"; Vunit ()
      
  (* Func *)
  and eval_func = ()

  (* Applic *)
  and eval_applic = ()

  (* If *)
  and eval_if = ()

  (* Pair *)
  and eval_pair = ()

  (* Let Pair *)
  and eval_let_pair = ()

  (* First *)
  and eval_first = ()

  (* Second *)
  and eval_second = ()

  
end