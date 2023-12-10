open Format

(* Type declares all non-primitive types. *)
module Type = struct
  
  module Quantifier = struct
    type t = string
    let make x = x
    let of_typeVar (tv, _) = tv
    let pp = Format.pp_print_string
    let compare = String.compare
  end

  type resolution_state = Unresolved | Resolved of t

  and typeVar = string * (resolution_state UnionFind.elem)

  and monoType = 
    | TypeInt
    | TypeBool
    | TypeString
    | TypeFunc of (t * t) (* A recursive definition refering to t. *)
    | TypePair of (t * t)
    | TypeUnit
    | TypeVar of typeVar
    
  and polyType = Quantifier.t list * monoType

  and t = monoType

  module TypeVar = struct
    let id_count = ref 0
    let reset () = id_count := 0
    
    let fresh_TV ?(scope_prefix="_") () =
      let id = !id_count in
      let () = incr id_count in
      let resolution_state = UnionFind.make Unresolved in
      (scope_prefix ^ (string_of_int id), resolution_state)

    let pp ppf (typeVar,_) = Format.pp_print_string ppf typeVar
    let var = fst
    let resolution_state = snd
    let compare (typeVar_a,_) (typeVar_b,_) = String.compare typeVar_a typeVar_b
  end

  let new_var () = TypeVar (TypeVar.fresh_TV ())

  let rec pp_monotype ppf = function
    | TypeInt -> Format.pp_print_string ppf "Int"
    | TypeBool -> Format.pp_print_string ppf "Bool"
    | TypeString -> Format.pp_print_string ppf "String"
    | TypeUnit -> Format.pp_print_string ppf "Unit"
    | TypeFunc (type_a, type_b) -> Format.fprintf ppf "(%a -> %a)" pp_monotype type_a pp_monotype type_b
    | TypePair (type_a, type_b) -> Format.fprintf ppf "(%a * %a)" pp_monotype type_a pp_monotype type_b
    | TypeVar tv -> TypeVar.pp ppf tv

  let pp_polytype ppf (quantifiers, monotype) = 
    if quantifiers = [] then 
      pp_monotype ppf monotype
    else
      let pp_space ppf () = pp_print_string ppf " " in
      let print_quantifiers ppf quantifiers = 
        pp_print_list ~pp_sep:pp_space Quantifier.pp ppf quantifiers
      in
      Format.fprintf ppf "forall %a. %a" print_quantifiers quantifiers pp_monotype monotype
    
  let pp = pp_monotype
    
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

(* Expr declares all valid expressions. *)
module Expr = struct
  type typ = Type.t
  type binder = string
  type variable = string

  type t = 
    | ExprVar of variable
    | ExprConst of Constant.t
    | ExprLet of (binder * t * t)
    | ExprOpBinary of (OpBinary.t * t * t)
    | ExprFunc of (binder * typ option * t)
    | ExprApplic of (t * t)
    | ExprAnn of (t * typ)
    | ExprIf of (t * t * t)
    | ExprPair of (t * t)
    | ExprLetPair of (variable * variable * t * t)
    | ExprFirst of t
    | ExprSecond of t
end

