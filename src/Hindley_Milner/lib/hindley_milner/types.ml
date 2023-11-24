open Format

module Type = struct
  
  module Quantifier = struct
    type t = string
    let make x = x
    let of_typeVar (tv, _) = tv
    let pp = Format.pp_print_string
    let compare = String.compare
  end

  type quantifier = Quantifier.t
  type resolution_state = Unresolved | Resolved of t
  and typeVar = string * (resolution_state UnionFind.elem) 
  and monoType = 
    | TypeInt
    | TypeBool
    | TypeString
    (* A recursive definition refering to t. *)
    | TypeFunc of (t * t)
    | TypePair of (t * t)
    | TypeUnit
    | TypeVar of typeVar
  and polyType = quantifier list * monoType
  and t = monoType

  module TypeVar = struct
    let source = ref 0
    let reset () = source := 0
    
    let new_var ?(prefix="_") () =
      let symbol = !source in
      let () = incr source in
      let point = UnionFind.make Unresolved in
      prefix ^ (string_of_int symbol), point

      let pp ppf (tv,_) = Format.pp_print_string ppf tv
      let var = fst
      let point = snd
      let compare (typeVar_a,_) (typeVar_b,_) = String.compare typeVar_a typeVar_b
  end

  let new_var () = TypeVar (TypeVar.new_var ())

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
        pp_print_list ~pp_sep:(pp_space) Quantifier.pp ppf quantifiers
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

