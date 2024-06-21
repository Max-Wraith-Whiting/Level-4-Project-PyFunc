module Type = struct
  type t = 
    | TypeInt
    | TypeBool
    | TypeString
    (* A recursive definition refering to t.*)
    | TypeFunc of (t * t)
    | TypeUnit

  let rec pp ppf = function
    | TypeInt -> Format.pp_print_string ppf "Int"
    | TypeBool -> Format.pp_print_string ppf "Bool"
    | TypeString -> Format.pp_print_string ppf "String"
    | TypeUnit -> Format.pp_print_string ppf "Unit"
    | TypeFunc (type_a, type_b) -> Format.fprintf ppf "(%a -> %a)" pp type_a pp type_b
end


(* OpBinary declares type for all possible binary operations. *)
module OpBinary = struct
  type t =
    | Add
    | Subtract
    | Multiply
    | Divide
    | Less
    | Greater
    | LessEqual
    | GreaterEqual
    | Equal
    | NotEqual
    | And
    | Or
end

(* Constant declares included primitive types. *)
module Constant = struct
  type t = 
  | ConstString of string
  | ConstBool of bool
  | ConstInt of int
  | ConstUnit
end

module Expr = struct
  type typ = Type.t
  type binder = string
  type variable = string

  type t = 
    | ExprVar of variable
    | ExprConst of Constant.t
    | ExprLet of (binder * t * t)
    | ExprOpBinary of (OpBinary.t * t * t)
    | ExprFunc of (binder * typ * t)
    | ExprApplic of (t * t)
    | ExprAnn of (t * typ)
    | ExprIf of (t * t * t)
end

