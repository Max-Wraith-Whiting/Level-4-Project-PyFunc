(* Start REPL *)

module REPL = struct
  module Type = HM.Ast.Type
  module Expr = HM.Ast.Expr
  module REPL_Parser = Ir.Parse
  module Typechecker = HM.Typechecker.Typecheck
  (* module Interpreter = Interpreter *)

  let reset_state = HM.Ast.Type.TypeVar.reset
(* 
  let generate_ir_ast source =
    REPL_Parser.parse_string source *)

  let repl ?(prompt="") () =
    reset_state ();
    print_string (prompt ^ "> ");
    let input_string = read_line () in
    let ast = Frontend.Frontend.generate_ast input_string in
    (* let converted_ast = Ir.Parse.parse_string input_string in
    let () = 
      try let tree_string = HM.Ast.Expr.print_tree converted_ast in
        print_endline "HM AST: " ;
        print_endline tree_string
      with
        | _ -> print_endline "AST print error!";
    in *)
    (* Print the AST. *)
    let () = 
      try let tree_string = Frontend.Frontend.pp_ast ast in
        print_endline "Initial AST:" ;
        print_string tree_string
      with 
        | _ -> print_endline "Something not right happened when printing AST!"
    in
    let converted_ast  = 
      try 
        let converted_ast = Frontend.Frontend.convert ast in
        print_endline "Printing Converted AST:";
        print_string (HM.Ast.Expr.print_tree converted_ast);
        converted_ast
      with
        | Frontend.Frontend.BadConversion msg -> print_endline msg; ExprConst (ConstUnit)
        | Frontend.Frontend.Unimplemented msg -> print_endline msg; ExprConst (ConstUnit)
        | _ -> print_endline "Somethine went wrong with IR conversion!"; ExprConst (ConstUnit)
    in
    
    (* Try to typecheck the AST. *)
    let () =
      try
        let typ = Typechecker.typecheck converted_ast in
        print_endline ("Type: " ^ (HM.Ast.Type.typ_to_string typ))
      with
        | HM.Errors.Parse_Error err -> print_endline ("[Parse error] " ^ err);
        | HM.Errors.Type_Error err -> print_endline ("[Type error] " ^ err);
        (* | Unsupported feat -> "[Unsupported] Feature %s is unsupported in language %s\n" feat prompt *)
        | exn -> print_endline ("[Error] " ^ (Printexc.to_string exn));
    in
    
    (* Try to interpret the converted and typechecked AST. *)
    let () = 
      try 
        let output = Interpreter.Interpreter.interpret converted_ast in
        print_endline ("Output: " ^ (Interpreter.pp_value output))
      with
        | Interpreter.Errors.Runtime_Error msg -> print_endline msg
        | Interpreter.Errors.Lookup_Error msg -> print_endline msg
        | x -> print_endline ("[ERROR]: " ^ (Printexc.to_string x))
    in
    ()
end

let () =
  while true do
    try REPL.repl () with
      | exn -> print_endline ("Error: " ^ (Printexc.to_string exn))
  done