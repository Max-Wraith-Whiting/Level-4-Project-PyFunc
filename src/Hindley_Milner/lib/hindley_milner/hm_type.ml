(* type op = ADD | SUB | GREATER | LESS | AND | OR

module CharacterMap = Map.Make(String)

type primitive_type =
  | Type_num
  | Type_bool
  | Type of string
  | Type_func of primitive_type * primitive_type
;;

type id = string

type expr = 
  | Literal_num of int
  | Literal_bool of bool
  | Value of string
  | Binary_op of expr * op * expr
  | Func of id * expr
  | Application of expr * expr
;;


let op_to_string (op: op): string = 
  match op with
  | ADD -> "+" 
  | SUB -> "-"
  | GREATER -> ">"
  | LESS -> "<"
  | OR -> "||"
  | AND -> "&&"


let rec expr_to_string (e: expr): string = 
  match e with
  | Literal_num x -> string_of_int x
  | Literal_bool b -> string_of_bool b
  | Value s -> s
  | Binary_op(e1, op, e2) ->
    let string_1 = expr_to_string e1
     and string_2 = expr_to_string e2
     in let op_string = op_to_string op 
    in "(" ^ string_1 ^ ", " ^ op_string ^ ", " ^ string_2 ^ ")"
  | Func(id, e) -> let string_1 = expr_to_string e in "(fun " ^ id ^ " -> " ^ string_1
  | Application(e1, e2) -> 
    let string_1 = expr_to_string e1 
     and string_2 = expr_to_string e2 
     in "(" ^ string_1 ^ ", " ^ string_2 ^ " )"
;;


let op_to_string (op: op): string = 
  match op with
  | ADD -> "+" 
  | SUB -> "-"
  | GREATER -> ">"
  | LESS -> "<"
  | OR -> "||"
  | AND -> "&&" *)

  module HMType = struct
    
    module Quantifier = struct
      type t = string
      let make x = x
      let ofTypeVar (typvar, _) = typvar
    end

    type quantifier = Quantifier.t
    type isResovled = Unresolved | Resolved of t
    and typeVar = string * (isResovled UnionFind.elem)
    and monotype =
      | TypeVar of typeVar
      | TypeInt
      | TypeBool
      | TypeString
      | TypeUnit
      | TypeFunc of (t * t)
    and polytype = quantifier list * monotype
    and t = monotype
end

(* module Expr = struct
  type t = HMType.t
  type binder = string
  type variable = string

  type t = 
    | ExprVar of variable
    | ExprConst of Constant.t
    | ExprLet of (binder * t * t)
    | ExprOpBinary of (Binary_op.t * t * t)
    | ExprFunc of (binder * typeDesc option * t)
    | ExprApp of (t * t)
    | ExprAnn of (t * typeDesc)
    | ExprIf of (t * t * t)
end *)