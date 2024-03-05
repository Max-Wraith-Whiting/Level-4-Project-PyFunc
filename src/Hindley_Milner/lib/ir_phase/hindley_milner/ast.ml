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
    | TypeList of t
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
    | TypeList l -> Format.fprintf ppf "[%a]" pp_monotype l

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

  let rec typ_to_string = function
    | TypeInt -> "Int"
    | TypeBool -> "Bool"
    | TypeString -> "String"
    | TypeUnit -> "Unit"
    | TypeFunc (type_a, type_b) -> "(" ^ (typ_to_string type_a) ^ " -> " ^ (typ_to_string type_b) ^ ")"
    | TypePair (type_a, type_b) -> "(" ^ (typ_to_string type_a) ^ " * " ^ (typ_to_string type_b) ^ ")"
    | TypeVar v -> TypeVar.var v
    | TypeList l -> "[" ^ (typ_to_string l) ^ "]"
    
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
    | Cons

  let op_binary_pp = function
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
    | Cons -> "::"
end

module OpUnary = struct
  type t = 
    | Positive
    | Negative
    | Not

  let op_unary_pp = function
    | Positive -> "+"
    | Negative -> "-"
    | Not -> "!"
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



  type tree = 
    | ExprVar of variable
    | ExprConst of Constant.t
    | ExprLet of (binder * tree * tree)
    | ExprLetRec of (binder * tree * tree) (* Binder, e, and e'*)
    | ExprOpUnary of (OpUnary.t * tree)
    | ExprOpBinary of (OpBinary.t * tree * tree)
    | ExprFunc of (binder * tree) (* Var binder, body*)
    | ExprApplic of (tree * tree) (* Arg, Func *)
    | ExprIf of (tree * tree * tree)
    | ExprPair of (tree * tree)
    | ExprLetPair of (variable * variable * tree * tree)
    | ExprFirst of tree
    | ExprSecond of tree
    | ExprList of tree list
    
  let rec get_name = function
    | ExprVar v -> v
    | ExprConst c -> Constant.pp c
    | ExprLet (v, _, _) ->  "Let: " ^ v
    | ExprLetRec (v, _, _) -> "Letrec " ^ v
    | ExprOpUnary (op, _) -> OpUnary.op_unary_pp op
    | ExprOpBinary (op, _, _) -> OpBinary.op_binary_pp op
    | ExprFunc (binder, _) -> "λ" ^ binder
    | ExprApplic _ -> "Apply"
    | ExprIf _ -> "If"
    | ExprPair _ -> "Pair"
    | ExprLetPair (a, b, _, _) -> "Let: (" ^ a ^ ", " ^ b ^ ")"
    | ExprList l -> 
      let rec join seperator = function
        | [] -> ""
        | [x] -> x
        | ""::xs -> join seperator xs
        | x::xs -> x ^ seperator ^ (join seperator xs)
      in
      "[" ^ (join ", " (List.map get_name l)) ^ "]"
    | _ -> ""
    
  let get_children = function
    | ExprVar _ -> []
    | ExprConst _ -> []
    | ExprLet (_, a, b) -> [a; b]
    | ExprLetRec (_, a, b) -> [a; b]
    | ExprOpUnary (_, a) -> [a;]
    | ExprOpBinary (_, a, b) -> [a; b]
    | ExprFunc (_, a) -> [a]
    | ExprApplic (a, b) -> [a; b]
    | ExprIf (cond, if_cond, else_cond) -> [cond; if_cond; else_cond]
    | ExprPair (a, b) -> [a; b]
    | ExprLetPair (_, _, a, b) -> [a; b]
    | _ -> []
  
  let print_tree tree =
    let rec iter fn = function
      | [] -> ()
      | [head] -> fn true head
      | head :: tail -> fn false head; iter fn tail
    in
    let open Printf in
    let buffer = Buffer.create 1000 in
    let to_buffer ?(line_prefix="") buffer tree = 
      let rec print_root indent tree = 
        bprintf buffer "%s\n" (get_name tree);
        iter (print_child indent) (get_children tree)
      and print_child indent is_last tree =
        let line = match is_last with
          | true  -> "└─"
          | false -> "├─"
        in
        bprintf buffer "%s%s" indent line;
        let extra_indent = match is_last with
          | true  -> "  "
          | false -> "│ "
        in
        print_root (indent ^ extra_indent) tree
        in
        Buffer.add_string buffer line_prefix;
        print_root line_prefix tree
      in
      to_buffer buffer tree;
      Buffer.contents buffer
    
end