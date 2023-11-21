(* Start REPL *)

let rec repl() = 
  print_string "> ";
  let user_input = read_line() in
  if user_input = "" then print_string "--- Closing REPL ---\n"
  else repl();;


(* repl();; *)

let expr_main = 
  let open Lexing in
  let lexbuf = Lexing.from_channel stdin in 
  let result = Pyfunc_frontend.Parse.main Lexer.token lexbuf in 
  Printf.printf "Result: %d\n" result