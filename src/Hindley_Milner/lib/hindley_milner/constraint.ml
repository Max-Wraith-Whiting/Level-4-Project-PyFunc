open Ast

module Constraint = struct
  type t = Type.t * Type.t

  let make type_a type_b = (type_a, type_b)
  let compare = Stdlib.compare
  let pp ppf (type_a, type_b) =
     Format.fprintf ppf "%a = %a" Type.pp type_a Type.pp type_b
end

module ConstraintSet = struct
  include Set.Make(Constraint)
  let union_many = List.fold_left (union) empty
  let make_singleton type_a type_b = singleton (Constraint.make type_a type_b)

  let pp ppf set =
    let pp_semi ppf () = Format.pp_print_string ppf "; " in
    let xs = elements set in
    Format.pp_print_list
      ~pp_sep:pp_semi
      Constraint.pp ppf xs
end

