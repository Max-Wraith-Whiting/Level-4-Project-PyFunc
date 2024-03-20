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

module Env = struct
  type t = (string * value) list
  and value = 
  | Vint of int 
  | Vfloat of float
  | Vbool of bool 
  | Vstring of string 
  | Vunit of unit 
  | Vvar of string 
  | VFn of (tree) 
  | VClos of (binder * tree * t)
  | Vref of value ref
  | Vpair of (value * value)
  | Vlist of value list

let rec pp_value = function
  | Vint i -> string_of_int i
  | Vfloat f -> string_of_float f
  | Vbool b -> string_of_bool b
  | Vstring s -> "\"" ^ s ^ "\""
  | Vunit _ -> "()"
  | Vvar var -> var
  | VFn t -> get_name t
  | VClos (a, _, _) -> a
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
  
  let make = ([] : t)

  let get (env : t) key =
    let result = 
      try List.assoc key env with 
      Not_found -> raise (Errors.lookup_error ("Undefined variable '" ^ key ^ "'!")) in
    result

  let set (env : t) (key : string) (value : value) = 
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
  open Env

  let eval_const = function
    | ConstInt i -> Vint i
    | ConstFloat f -> Vfloat f
    | ConstBool b -> Vbool b
    | ConstString s -> Vstring s
    | ConstUnit -> Vunit ()

  let rec eval env = 
    function
    | ExprConst c -> eval_const c 
    | ExprVar var -> eval_var env var
    | ExprOpUnary (op, expr) -> eval_op_unary env op expr
    | ExprOpBinary (op, expr_a, expr_b) -> eval_op_binary env op expr_a expr_b
    | ExprIf (condition, expr_a, expr_b) -> eval_if env condition expr_a expr_b
    | ExprLet (binder, value, expr) -> eval_let env binder value expr
    | ExprLetRec (binder, value, expr) -> eval_letrec env binder value expr
    | ExprApplic (arg, func) -> eval_applic env arg func 
    | ExprFunc (binder, body) -> eval_func env binder body (* First-order functions can be values. *)
    | ExprPair (first, second) -> eval_pair env first second
    | ExprLetPair (binder_a, binder_b, expr_a, expr_b) -> eval_let_pair env binder_a binder_b expr_a expr_b
    | ExprFirst (ExprPair (first, _)) -> eval env first 
    | ExprSecond (ExprPair (_, second)) -> eval env second
    | ExprList list -> eval_list env list
    | _ -> print_endline "Oh no! Invalid tree node!"; (Vunit ())

  and eval_op_unary env op expr = 
    let expr_value = eval env expr in

    let rec value_to_int = function
    | Vint i -> Vint i
    | Vfloat f -> Vint (Float.to_int f)
    | Vbool b -> Vint (Bool.to_int b)
    | Vpair (a, b) -> Vpair (value_to_int a, value_to_int b)
    | Vlist l -> Vlist (List.map (value_to_int) l)
    | Vunit () -> Vunit ()
    | _ -> raise (Errors.runtime_error "Illegal input to unary int cast!")
    in

    let rec value_to_float = function
    | Vint i -> Vfloat (Int.to_float i)
    | Vfloat f -> Vfloat f
    | Vbool b -> Vfloat (Bool.to_float b)
    | Vpair (a, b) -> Vpair (value_to_float a, value_to_float b)
    | Vlist l -> Vlist (List.map (value_to_float) l)
    | Vunit () -> Vunit ()
    | _ -> raise (Errors.runtime_error "Illegal input to unary float cast!")
    in

    let rec value_to_string = function
    | Vint i -> Vstring (Int.to_string i)
    | Vfloat f -> Vstring (Float.to_string f)
    | Vbool b -> Vstring (Bool.to_string b)
    | Vpair (a, b) -> Vpair (value_to_string a, value_to_string b)
    | Vlist l -> Vlist (List.map (value_to_string) l)
    | VClos (binder, _, _) -> Vstring (binder)
    | Vunit () -> Vunit ()
    | _ -> raise (Errors.runtime_error "Illegal input to unary string cast!")
    in

    let rec value_to_bool = function
    | Vint i -> Vbool (if i = 0 then false else true)
    | Vfloat f -> Vbool (if f = 0. then false else true)
    | Vbool b -> Vbool b
    | Vstring s -> Vbool (if s = "" then false else true)
    | Vpair (a, b) -> Vpair (value_to_bool a, value_to_bool b)
    | Vlist l -> Vlist (List.map (value_to_bool) l)
    | Vunit () -> Vunit ()
    | _ -> raise (Errors.runtime_error "Illegal input to unary float cast!")
    in

    match op, expr_value with
      | Positive, (Vint i) -> Vint (~+i)
      | Negative, (Vint i) -> Vint (~-i)
      | Not, (Vbool b) -> Vbool (not b)
      | Head, (Vlist l) -> List.hd l
      | Tail, (Vlist l) -> Vlist(List.tl l)
      | UInt, v -> value_to_int v
      | UFloat, v -> value_to_float v
      | UBool, v -> value_to_bool v
      | UString, v -> value_to_string v
      | _, _ -> raise (Errors.runtime_error ("Oh no! Invalid unary op: " ^ op_unary_pp op))

  and eval_op_binary env op expr_a expr_b =
    let left = eval env expr_a in
    let right = eval env expr_b in
    print_endline ("op_binary: " ^ (Env.pp_value left) ^ " " ^ (HM.Ast.OpBinary.op_binary_pp op) ^ " " ^ (Env.pp_value right));

    let modulo x y = 
      let remainder = x mod y in
        if remainder >= 0 then 
          remainder
        else
          remainder + y
      in

    match left, op, right with
    (* Integer operations *)
      | (Vint a), Add,       (Vint b) ->  Vint (a + b)
      | (Vint a), Subtract,  (Vint b) ->  Vint (a - b)
      | (Vint a), Multiply,  (Vint b) ->  Vint (a * b)
      | (Vint a), IntDivide, (Vint b) -> Vint ( a / b)
      | (Vint a), Divide,    (Vint b) -> Vfloat (float_of_int a /. float_of_int b)
      | (Vint a), Mod,       (Vint b) ->  Vint (modulo a b)
      | (Vint a), Exponent,  (Vint b) -> Vint (Float.to_int (Float.pow (Int.to_float a) (Int.to_float b)))
    (* Float operations *)
      | (Vfloat a), Add,      (Vfloat b) ->  Vfloat (a +. b)
      | (Vfloat a), Subtract, (Vfloat b) ->  Vfloat (a -. b)
      | (Vfloat a), Multiply, (Vfloat b) ->  Vfloat (a *. b)
      | (Vfloat a), Divide,   (Vfloat b) ->  Vfloat (a /. b)
      | (Vfloat a), Exponent, (Vfloat b) ->  Vfloat (Float.pow a b)
    (* Comparitive Int operations *)
      | (Vint a), Less,         (Vint b) -> Vbool (a < b)
      | (Vint a), Greater,      (Vint b) -> Vbool (a > b)
      | (Vint a), LessEqual,    (Vint b) -> Vbool (a <= b)
      | (Vint a), GreaterEqual, (Vint b) -> Vbool (a >= b)
      | (Vint a), Equal,        (Vint b) -> Vbool (a = b)
      | (Vint a), NotEqual,     (Vint b) -> Vbool (a <> b)
    (* Comparative Float Operations *)
      | (Vfloat a), Less,         (Vfloat b) -> Vbool (a < b)
      | (Vfloat a), Greater,      (Vfloat b) -> Vbool (a > b)
      | (Vfloat a), LessEqual,    (Vfloat b) -> Vbool (a <= b)
      | (Vfloat a), GreaterEqual, (Vfloat b) -> Vbool (a >= b)
      | (Vfloat a), Equal,        (Vfloat b) -> Vbool (a = b)
      | (Vfloat a), NotEqual,     (Vfloat b) -> Vbool (a <> b)
    (* Comparitive Boolean Operations *)
      | (Vbool a), Equal,    (Vbool b) -> Vbool (a = b)
      | (Vbool a), NotEqual, (Vbool b) -> Vbool (a <> b)
    (* Logical operations *)
      | (Vbool a), And, (Vbool b) -> Vbool (a && b)
      | (Vbool a), Or,  (Vbool b) -> Vbool (a || b)
    (* List Operations *)
      | (a), Cons, (Vlist b) -> Vlist (a :: b)
      | _, op, _ -> raise (Errors.runtime_error ("Oh no! Invalid binary op: " ^ op_binary_pp op))

  and eval_if env condition expr_a expr_b =
    let cond = eval env condition in
    match cond with
      | Vbool true -> eval env expr_a
      | Vbool false -> eval env expr_b
      | Vvar var -> eval_var env var
      | _ -> raise (Errors.runtime_error "Oh no! Invalid condition: Must be a boolean!")

  and eval_let env binder let_expr in_expr =
    (* Compute value *)
    let value = eval env let_expr in
    (* Set (binder, value) in environment. *)
    let env' = Env.set env binder value in
    (* Eval expr with new environment. *)
    eval env' in_expr

  and eval_letrec env binder let_expr in_expr =
    print_endline ("letrec: " ^ binder);
    (* Set variable into the Env before evaluation. *)
    let fn = VFn (let_expr) in
    let env' = (Env.set env binder fn) in
    (* Compute value with recursive referencing. *)
    eval env' in_expr

  and eval_var env var =
    (* Attempt to get var from env *)
    try let result = Env.get env var in
    match result with
      | VFn body -> print_endline ("VFn var: " ^ var); eval env body
      | VClos (binder, body, env') -> print_endline ("VClos var: " ^ binder); eval env' body
      | x -> x

    with
      | Errors.Lookup_Error msg -> raise (Errors.Lookup_Error msg)
      | exn -> print_endline ("Error: " ^ (Printexc.to_string exn)); raise exn

  and eval_func env binder body = 
    print_endline ("Fn: " ^ binder);
    VClos (binder, body, env)

  and eval_applic env func arg =
      (* Attmept computation. *)
      let fn = eval env func in
      (* Unwrap value. *)
      match fn with 
      | VFn body -> eval env body
      | VClos (binder, body, c_env) -> 
        let arg_value = eval env arg in
      (* Bind arg *)
        print_endline ("Applic fn: " ^ binder);
        let env' = Env.set c_env binder arg_value in
        eval env' body 
      | v -> v

  and eval_pair env first second =
    (* print_endline "eval_pair"; *)
    (* Evaluate the terms of the pair, but retain the structure. *)
    let value_first = eval env first in
    let value_second = eval env second in
      Vpair (value_first, value_second)

  
  and eval_let_pair env binder_a binder_b definition scope =  
    (* let env = !ref_env in  *)
    let pair_definition = 
    match eval env definition with
      | Vpair (a, b) -> (a, b)
      | _ -> raise (Errors.runtime_error ("Let-Pair definition does not return a pair value!"))
    in
    (* Extract the pair values. *)
    let (value_a, value_b) = pair_definition in
    (* Set binder_a to value_a and binder_b to value_b. *)
    let env' = Env.set env binder_a value_a in
    let env'' = Env.set env' binder_b value_b in
      (* ref_env := env''; *)
      eval env'' scope


  and eval_list env list =
    let value_list = List.map (eval env) list in
    Vlist value_list

  let interpret (root_node : tree) =
    eval Env.make root_node
end