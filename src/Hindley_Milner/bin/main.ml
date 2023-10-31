(* Start REPL *)

let rec repl() = 
  print_string "> ";
  let user_input = read_line() in
  if user_input = "" then print_string "--- Closing REPL ---\n"
  else repl();;


repl();;