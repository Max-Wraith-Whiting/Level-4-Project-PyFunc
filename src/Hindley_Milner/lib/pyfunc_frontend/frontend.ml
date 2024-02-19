open Py_ast.Expr

module Frontend = struct
  module Lexer = Py_lexer
  module Parser = Py_parser
  module IR = HM.Ast.Expr

  let pp_ast ast = Py_ast.Expr.print_tree ast

  let generate_ast source = 
    let open Parse in
    parse_string source

  exception Unimplemented of string
  let raise_unimpl msg = 
    raise (Unimplemented msg)

  let convert = function
    | Const c -> IR.ExprConst c
    | _ -> raise_unimpl "Not implemented currently!"

end