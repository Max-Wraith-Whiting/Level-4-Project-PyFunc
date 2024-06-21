(* Start REPL *)

module Main = struct
  module Type = HM.Ast.Type
  module Expr = HM.Ast.Expr
  module REPL_Parser = Ir.Parse
  module Typechecker = HM.Typechecker.Typecheck
  (* module Interpreter = Interpreter *)

  let reset_state = HM.Ast.Type.TypeVar.reset

  let print_frontend_ast ast verbose =
    if !verbose = true then (
    try let tree_string = Frontend.Frontend.pp_ast ast in
      print_endline "Initial AST:" ;
      print_string tree_string
    with 
      | _ -> print_endline "Something not right happened when printing AST!"
    )
    else ()
  
  let print_ir_ast ast verbose = 
    if !verbose = true then
      (
        print_endline "Converted AST:";
        print_string (HM.Ast.Expr.print_tree ast);
      )
    else ()


  let execute_code (source : string list) verbose = 
    let ast = Frontend.Frontend.generate_ast source in
      (* Print the AST. *)
      print_frontend_ast ast verbose;
      let converted_ast  = 
        try 
          let converted_ast = Frontend.Frontend.convert ast in
          print_ir_ast converted_ast verbose;
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
          print_endline ("Output: " ^ (Interpreter.Env.pp_value output))
        with
          | Interpreter.Errors.Runtime_Error msg -> print_endline msg
          | Interpreter.Errors.Lookup_Error msg -> print_endline msg
          | x -> print_endline ("[ERROR]: " ^ (Printexc.to_string x))
      in
      ()

  let rec read_multiline_input acc =
    let line = read_line () in
    if line = "" then
      List.rev acc
    else
      read_multiline_input (line :: acc)

  let repl ?(prompt="") verbose =
    reset_state ();
    print_string (prompt ^ "> ");
    let input_string = read_multiline_input [] in
    execute_code input_string verbose
end

let run_file name verbose = 
  let read_lines name : string list =
    let ic = open_in name in
    let try_read () =
      try Some (input_line ic) with End_of_file -> None in
    let rec loop acc = match try_read () with
      | Some s -> loop (s :: acc)
      | None -> close_in ic; List.rev acc in
    loop []
  in
  let lines = read_lines name in
  try Main.execute_code lines verbose with
    | exn -> print_endline ("Error: " ^ (Printexc.to_string exn))


let usage_msg = "pyfunc [-verbose] [<file>]"

let verbose = ref false

let input_file = ref []

let anon_func filename =
  input_file := filename :: !input_file

let speclist = 
  [("-verbose", Arg.Set verbose, "Output debug information")]

let run_repl () = 
  while true do
      try Main.repl verbose with
        | exn -> print_endline ("Error: " ^ (Printexc.to_string exn))
    done

let () =
  Arg.parse speclist anon_func usage_msg;
  if (List.is_empty !input_file) then
    (print_endline ("--------PyFunc v0.1 REPL--------");
    run_repl ())
  else
    (* print_endline *)
    run_file (List.hd !input_file) verbose