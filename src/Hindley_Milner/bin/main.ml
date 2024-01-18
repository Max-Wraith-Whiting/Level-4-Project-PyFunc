(* Start REPL *)

(* open Pyfunc_frontend *)
(* open HM.Typechecker *)
(* open HM.Ast *)
(* open HM.Errors *)
open Pyfunc_frontend
(* 
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
end *)

(* module ASTPrint = struct
  (* module Type = HM.Ast.Type *)
  module Expr = HM.Ast.Expr

  let rec repl ?(prompt="") () = 
    print_string (prompt ^ "> ");
    let string = read_line () in
      (* try *)
        let ast = Interpreter.ast_parse string in
        ast
      (* with
        | Parse_Error err -> Format.printf "[Parse error] %s \n" err
        | Type_Error err -> Format.printf "[Type error] %s \n" err
        | exn -> Format.printf "[Error] %s\n" (Printexc.to_string exn) *)
    let () = Format.print_flush () in
    repl ~prompt ()
end *)

(* module Repl = ASTPrint;;
Repl.repl () *)

(* let f str = 
  let ast = Interpreter.ast_parse str in
    HM.Ast.Expr.pp ast;
    print_string(HM.Ast.Expr.print_tree ast);;
    (* print_string("\n" ^ string_of_int(Interpreter.interpret ast) ^ "\n");; *)



f (print_string("> "); read_line ());; *)

let ast_parse str = 
  let lexbuf = Lexing.from_string str in
  let ast = Parser.main Lexer.token lexbuf in
    ast
;;



print_string(Ast.Expr.print_tree (ast_parse("if True: # Hello world!\n 1 else 2")))
  (* print_string(Ast.Expr.print_tree ast);; *)
  