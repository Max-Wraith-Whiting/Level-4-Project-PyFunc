open Primative_types

(* Public interface for type t
   Here for expansion. *)
module type TYPE = sig
  type t
end

(* Declared names to clarify code. *)
type binder = string
type variable = string

(* EXPR refers to an express. They hold 3 things:
   - A type.
   - A binder: In an expression λx.M, λx is called the "binder".
   - A variable: The value. *)
module type EXPR = sig
  type t
  type binder = string
  type variable = string
end

module type LANGUAGE = sig
  module Type : TYPE
  module Expr : EXPR
(* TypeConstructors interface, for making type data. *)
  module TypeConstructors : sig
    val make_int : Type.t
    val make_bool : Type.t
    val make_string : Type.t
    val make_unit : Type.t
    val make_func : Type.t -> Type.t -> Type.t
    val make_pair : Type.t -> Type.t -> Type.t
  end

  (* ExprConstructors interface, for making expressions. *)
  module ExprConstructors : sig
    val make_constant : Constant.t -> Expr.t
    val make_variable : variable -> Expr.t
    val make_let : binder -> Expr.t -> Expr.t -> Expr.t
    val make_op_binary : OpBinary.t -> Expr.t -> Expr.t -> Expr.t
    val make_func : binder -> Type.t option -> Expr.t -> Expr.t
    val make_application : Expr.t -> Expr.t -> Expr.t
    val make_annotation : Expr.t -> Type.t -> Expr.t
    val make_if : Expr.t -> Expr.t -> Expr.t -> Expr.t
    val make_pair : Expr.t -> Expr.t -> Expr.t 
    val make_first : Expr.t -> Expr.t 
    val make_second : Expr.t -> Expr.t 
    val make_let_pair : variable -> variable -> Expr.t -> Expr.t -> Expr.t 
  end

  (* Besides the constructors, LANGUAGE allows typecheck and state-mangement.*)
  val typecheck : Expr.t -> Type.t
  val reset_state : unit -> unit
end