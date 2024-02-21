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

module Expr = struct
  type variable = string

  type binder = string
  type param_list = string list
  
  type tree =
  | Var of variable (* Just a variable in use. *)
  | Const of HM.Ast.Constant.t
  | If of (tree * tree * tree) (* Condition, If-true, If-false *)
  | Param of binder (* A specified input variable. *)
  | Func of (binder * param_list * tree) (* Func = (ID * Params * function_expr) This should be loaded from the environment. *)
  | Call of (variable * tree list) (* Function call. Call = (Func * Args)*)
  | Assignment of (binder * tree * tree) (* Let binder = tree_1 in tree_2. Tree_2 is in effect the scope of the variable assignment. *)
  | OpBinary of (OpBinary.t * tree * tree)
  
  
  let get_name = function
    (* | Program _ -> "Main" *)
    | Const c -> HM.Ast.Constant.pp c
    | Var v -> v
    | OpBinary (op, _, _) -> OpBinary.pp op
    | Func (binder, params, _) -> "Func: " ^ binder ^ " Params: " ^ (String.concat ", " params)
    (* | Args _ -> "args" *)
    | Param _ -> "params"
    | Call (v, _) -> "Call: " ^ v
    | If _ -> "If"
    | Assignment (v, _, _) -> "Assign: " ^ v
    
  let get_children = function
    (* | Program a -> [a] *)
    | Const _ -> []
    | Var _ -> []
    | OpBinary (_, a, b) -> [a; b]
    | Func (_, _, a) -> [a]
    (* | Args a -> a *)
    | Param _ -> []
    | Call (_, a) -> a
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
end


module Constructor = struct
  let makeConst const = Expr.Const const
  let makeIf condition if_expr else_expr = Expr.If (condition, if_expr, else_expr)
  let makeOpBinary op expr_a expr_b = Expr.OpBinary (op, expr_a, expr_b)
  let makeDefineFunc id param func_expr = Expr.Func (id, param, func_expr)
  let makeCall id args = Expr.Call (id, args) (* Call should load a ref to func Def *)
  let makeAssign id value scope = Expr.Assignment (id, value, scope)
  let makeParam alias = Expr.Param alias
  let makeVar var = Expr.Var var
  let makeFunc id args expr = Expr.Func (id, args, expr)
end
