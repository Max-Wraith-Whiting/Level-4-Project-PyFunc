(* Start REPL *)

open HM.Errors
open HM.Typechecker
open HM.Ast.Type
open Ir
open Interpreter

module REPL = struct
  module Type = HM.Ast.Type
  module Expr = HM.Ast.Expr
  module REPL_Parser = Parse
  module Typechecker = Typecheck
  (* module Interpreter = Interpreter *)

  let _typecheck = Typechecker.typecheck
  let reset_state = TypeVar.reset

  let type_check ast = 
    print_string (HM.Ast.Expr.print_tree ast);
      _typecheck ast

  let generate_ast source =
    REPL_Parser.parse_string source

  let repl ?(prompt="") () =
    reset_state ();
    print_string (prompt ^ "> ");
    let input_string = read_line () in
    let ast = generate_ast input_string in
    let () =
      try
        let typ = type_check ast in
        Format.printf "Type: %a\n" HM.Ast.Type.pp typ
      with
        | Parse_Error err -> Format.printf "[Parse error] %s \n" err
        | Type_Error err -> Format.printf "[Type error] %s \n" err
        (* | Unsupported feat -> "[Unsupported] Feature %s is unsupported in language %s\n" feat prompt *)
        | exn -> Format.printf "[Error] %s\n" (Printexc.to_string exn)
    in
    let () =
    Format.print_flush ();
    print_string ("Output: " ^ (pp_value (Interpreter.interpret ast)) ^ "\n")
    in
    ()
end

let () =
  while true do
    try REPL.repl () with
      | _ -> ()
  done

(* print_string (Frontend.get_ast "if x == \"94\": {zz(a , 15   , 1992, chevolette)} else {1992}") *)