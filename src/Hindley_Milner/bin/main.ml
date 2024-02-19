(* Start REPL *)

(* open HM.Errors *)
open HM.Typechecker
open HM.Ast.Type
open Ir
(* open Interpreter *)
open Frontend

module REPL = struct
  module Type = HM.Ast.Type
  module Expr = HM.Ast.Expr
  module REPL_Parser = Parse
  module Typechecker = Typecheck
  (* module Interpreter = Interpreter *)

  let reset_state = TypeVar.reset
(* 
  let generate_ir_ast source =
    REPL_Parser.parse_string source *)

  let repl ?(prompt="") () =
    reset_state ();
    print_string (prompt ^ "> ");
    let input_string = read_line () in
    let ast = Frontend.generate_ast input_string in

    (* Print the AST. *)
    let () = 
      try let tree_string = Frontend.pp_ast ast in
        print_endline "Initial AST:" ;
        print_string tree_string
      with 
        | _ -> print_endline "Something not right happened when printing AST!"
    in
    let () = 
      try 
        let converted_ast = Frontend.convert ast in
        print_endline "Printing Converted AST:";
        print_string (HM.Ast.Expr.print_tree converted_ast)
      with
        | _ -> print_endline "Somethine went wrong with IR conversion!"
    in
(*     
    (* Try to type-check the AST. *)
    let () =
      try
        let typ = Typechecker.typecheck ast in
        print_endline ("Type: " ^ (HM.Ast.Type.typ_to_string typ))
      with
        | Parse_Error err -> print_endline ("[Parse error] " ^ err);
        | Type_Error err -> print_endline ("[Type error] " ^ err);
        (* | Unsupported feat -> "[Unsupported] Feature %s is unsupported in language %s\n" feat prompt *)
        | exn -> print_endline ("[Error] " ^ (Printexc.to_string exn));
    in
    (* Format.print_flush (); *)
    print_string ("Output: " ^ (pp_value (Interpreter.interpret ast)) ^ "\n") *)
    ()
end

let () =
  while true do
    try REPL.repl () with
      | exn -> print_endline ("Error: " ^ (Printexc.to_string exn))
  done