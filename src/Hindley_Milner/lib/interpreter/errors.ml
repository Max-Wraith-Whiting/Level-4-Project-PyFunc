(* Basic runtime error management *)
exception Runtime_Error of string 
exception Lookup_Error of string

let raise_error msg = Runtime_Error msg
let raise_lookup_error msg = Lookup_Error msg