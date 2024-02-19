module Frontend = struct
  module Lexer = Py_lexer
  module Parser = Py_parser
  module Ast = Py_ast

  let pp_ast ast = Ast.Expr.print_tree ast

  let generate_ast source = 
    let open Parse in
    parse_string source

end