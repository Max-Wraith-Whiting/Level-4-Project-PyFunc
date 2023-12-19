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

  type quantifier = Quantifier.t
  type resolution_state = Unresolved | Resolved of t

  and typevar = string * (resolution_state UnionFind.elem)
  and monoType = 
    | TypeInt
    | TypeBool
    | TypeString
    | TypeFunc of (t * t) (* A recursive definition refering to t. *)
    | TypePair of (t * t)
    | TypeUnit
    | TypeVar of typevar
  and polyType = quantifier list * monoType
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

  let pp = function
    | Add -> "+"
    | Subtract -> "-" 
    | Multiply -> "*"
    | Divide -> "÷"
    | Less -> "<"
    | Greater -> ">"
    | LessEqual -> "<="
    | GreaterEqual -> ">="
    | Equal -> "=="
    | NotEqual -> "!="
    | And -> "And"
    | Or -> "Or"
end

(* Constant declares included primitive types. *)
module Constant = struct
  type t = 
    | ConstString of string
    | ConstBool of bool
    | ConstInt of int
    | ConstUnit

  let pp = function
    | ConstString s -> s
    | ConstBool b -> string_of_bool b
    | ConstInt i -> string_of_int i
    | ConstUnit -> "()"
end

(* Expr declares all valid expressions. *)
module Expr = struct
  type typ = Type.t
  type binder = string
  type variable = string

  type expr_tree = 
    | ExprVar of variable
    | ExprConst of Constant.t
    | ExprLet of (binder * expr_tree * expr_tree)
    | ExprOpBinary of (OpBinary.t * expr_tree * expr_tree)
    | ExprFunc of (binder * typ option * expr_tree)
    | ExprApplic of (expr_tree * expr_tree)
    | ExprAnn of (expr_tree * typ)
    | ExprIf of (expr_tree * expr_tree * expr_tree)
    | ExprPair of (expr_tree * expr_tree)
    | ExprLetPair of (variable * variable * expr_tree * expr_tree)
    | ExprFirst of expr_tree
    | ExprSecond of expr_tree

  let rec print_ast acc = function
    | ExprVar v -> acc ^ v
    | ExprConst c -> acc ^ (Constant.pp c)
    | ExprLet (binder, a, b) -> acc ^ "Let:" ^ binder ^ (print_ast acc a) ^ (print_ast acc b) ^ " "
    | ExprOpBinary (op, a, b) -> acc ^ (print_ast acc a) ^ " " ^ (OpBinary.pp op) ^ " " ^ (print_ast acc b)
    | ExprFunc (binder, _, a) -> acc ^ "λ (" ^ binder ^ ") (" ^ (print_ast acc a) ^ ")"
    | ExprApplic (a, b) -> acc ^ "Apply (" ^ (print_ast acc a) ^ ") (" ^ (print_ast acc b) ^ ") "
    | ExprAnn _ -> acc
    | ExprIf (cond, if_cond, else_cond) -> acc ^ "If:" ^ (print_ast acc cond) ^ " " ^ (print_ast acc if_cond) ^ " " ^ (print_ast acc else_cond) ^ " "
    | ExprPair (a, b) -> acc ^ "Pair:" ^ (print_ast acc a) ^ ", " ^ (print_ast acc b) ^ " "
    | _ -> ""

  let get_name = function
    | ExprVar v -> v
    | ExprConst c -> Constant.pp c
    | ExprLet (binder, _, _) ->  binder
    | ExprOpBinary (op, _, _) -> OpBinary.pp op
    | ExprFunc _ -> "λ"
    | ExprApplic _ -> "Apply"
    | ExprAnn _ -> ""
    | ExprIf _ -> "If"
    | ExprPair _ -> "Pair"
    | _ -> ""

  let get_children = function
    | ExprVar v -> []
    | ExprConst c -> []
    | ExprLet (_, a, b) -> [a; b]
    | ExprOpBinary (_, a, b) -> [a; b]
    | ExprFunc (_, _, a) -> [a]
    | ExprApplic (a, b) -> [a; b]
    | ExprAnn _ -> [] 
    | ExprIf (cond, if_cond, else_cond) -> [cond; if_cond; else_cond]
    | ExprPair (a, b) -> [a; b]
    | _ -> []

  let pp x = 
    let p = Format.pp_print_string (Format.get_std_formatter ()) in
    p (print_ast "" x)
end