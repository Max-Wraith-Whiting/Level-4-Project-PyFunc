(* open HM.Ast.OpBinary *)

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

module Expr = struct
  type variable = string

  type binder = string

  type tree =
    | Program of tree (* Program wrapper. *)
    | Var of variable
    | Const of Constant.t
    | If of (tree * tree * tree) (* Condition, If-true, If-false *)
    | Args of tree (* A passed value to a function. *)
    | Param of binder (* A specified input variable. *)
    | Func of (binder * binder * tree) (* Func = (ID * Params * function_expr) This should be loaded from the environment. *)
    | Call of (variable * tree) (* Function call. Call = (Func * Args)*)
    (* | Expr of tree *)
    | Assignment of (binder * tree * tree) (* Let binder = tree_1 in tree_2. Tree_2 is in effect the scope of the variable assignment. *)
    | OpBinary of (OpBinary.t * tree * tree)


  let get_name = function
    | Program _ -> "Main"
    | Const c -> Constant.pp c
    | Var v -> v
    | OpBinary (op, _, _) -> OpBinary.pp op
    | Func (binder, _, _) -> binder ^ "()"
    | Args _ -> "args"
    | Param _ -> "params"
    | Call (v, _) -> "Call " ^ v 
    | If _ -> "If"
    | Assignment (v, _, _) -> "Assign " ^ v
    
  let get_children = function
    | Program a -> [a]
    | Const _ -> []
    | Var _ -> []
    | OpBinary (_, a, b) -> [a; b]
    | Func (_, _, a) -> [a]
    | Args a -> [a]
    | Param _ -> []
    | Call (_, a) -> [a]
    | If (c, a, b) -> [c; a; b]
    | Assignment (_, a, b) -> [a; b]
    
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
    

  (* let rec print_ast acc = function
    | Var v -> acc ^ v
    | Const c -> acc ^ (Constant.pp c)
    | OpBinary (op, a, b) -> acc ^ (print_ast acc a) ^ " " ^ (OpBinary.pp op) ^ " " ^ (print_ast acc b)
    | Func (binder, p, a) -> acc ^ "def " ^ binder ^ " (" ^ p ^ "):" ^ (print_ast acc a) ^ ""
    | If (cond, if_cond, else_cond) -> acc ^ "If " ^ (print_ast acc cond) ^ ":\n    " ^ (print_ast acc if_cond) ^ "\n" ^ (print_ast acc else_cond) ^ " "
    | Args t -> acc ^ (print_ast acc t)
    | Param s -> acc ^ s
    | Assignment (vn, a, b) -> acc ^ vn ^ " = " ^ (print_ast acc a) ^ " in " ^ (print_ast acc b)
    | _ -> ""

    let prp x = 
      Format.pp_print_string (Format.get_std_formatter ()) ((print_ast "" x) ^ "\n") *)
end


module Constructor = struct
  let makeConst const = Expr.Const const
  let makeIf condition if_expr else_expr = Expr.If (condition, if_expr, else_expr)
  let makeOpBinary op expr_a expr_b = Expr.OpBinary (op, expr_a, expr_b)
  let makeDefineFunc id param func_expr = Expr.Func (id, param, func_expr)
  let makeCall id args = Expr.Call (id, args) (* Call should load a ref to func Def *)
  let makeAssign id value scope = Expr.Assignment (id, value, scope)
  let makeParam alias = Expr.Param alias
  let makeArgs value = Expr.Args value
  let makeVar var = Expr.Var var
  let makeFunc id param expr = Expr.Func (id, param, expr)
end
