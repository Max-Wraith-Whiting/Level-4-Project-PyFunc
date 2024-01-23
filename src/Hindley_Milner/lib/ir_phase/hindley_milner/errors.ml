exception Type_Error of string
exception Parse_Error of string

let type_error str = Type_Error str
let parse_error str = Parse_Error str