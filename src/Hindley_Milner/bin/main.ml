(* Start REPL *)
open Pyfunc_frontend
open HM.Typechecker
open HM.Errors
open HM.Types

let pipe str = 
  let expr = Parse.parse_string str in Typecheck.typecheck expr

let rec repl () = 
  print_string "> ";
  let user_input = read_line() in
  (* if user_input = "" then print_string "--- Closing REPL ---\n" *)
  (* else  *)
  let () = 
    try
      let typ = pipe user_input in
      Format.printf "Type: %a\n" Type.pp typ
    with 
    | Parse_Error err -> Format.printf "[Parse Error] %S\n" err
    | Type_Error err ->  Format.printf "[Type Error] %s\n" err
    in 
    let () = Format.print_flush () in
      repl ();;


repl ();;
(* 
let expr_main = 
  let open Lexing in
  let lexbuf = Lexing.from_channel stdin in 
  let result = Parse.parse_string Lexer.token lexbuf in 
  Printf.printf "Result: %d\n" result *)

