(* Constant declares included primative types. *)
module Contant = struct
  type t = 
  | ConstString of string
  | ConstBool of bool
  | ConstInt of int
  | ConstUnit
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