(* Start REPL *)

open HM.Errors
open HM.Typechecker
open HM.Ast.Type
open Ir

module REPL = struct
  module Type = HM.Ast.Type
  module Expr = HM.Ast.Expr
  module REPL_Parser = Parse
  module Typechecker = Typecheck

  let typecheck = Typechecker.typecheck
  let reset_state = TypeVar.reset

  let process string = 
    let expr = REPL_Parser.parse_string string in
      (* print_string ("[Input]:" ^ Expr.pp expr ^ "\n"); *)
      typecheck expr

  let rec repl ?(prompt="") () =
    reset_state ();
    print_string (prompt ^ "> ");
    let string = read_line () in
    let () =
      try
        let typ = process string in
        Format.printf "Type: %a\n" HM.Ast.Type.pp typ
      with
        | Parse_Error err -> Format.printf "[Parse error] %s \n" err
        | Type_Error err -> Format.printf "[Type error] %s \n" err
        (* | Unsupported feat -> "[Unsupported] Feature %s is unsupported in language %s\n" feat prompt *)
        | exn -> Format.printf "[Error] %s\n" (Printexc.to_string exn)
    in
    let () = Format.print_flush () in 
    repl ~prompt ()
end

let () =
  REPL.repl ()

(* print_string (Frontend.get_ast "if x == \"94\": {zz(a , 15   , 1992, chevolette)} else {1992}") *)