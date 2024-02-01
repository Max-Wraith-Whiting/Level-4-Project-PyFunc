(* Basic runtime error management *)
exception Runtime_Error of string 

let raise_error msg = Runtime_Error msg