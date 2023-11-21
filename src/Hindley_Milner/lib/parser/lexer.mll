{
    open Parser
    open Lexing

    exception Lexical_error of string
    let lexical_error msg = Lexical_error msg

}
(* NEED TO DEFINE parse function*)
rule token = parse
    | [' ' '\t' '\r' '\n']* { token lexbuf }
    | "\\"      {LAMBDA}
    | "let"     {LET}
    | "in"      {IN}
    | "true"    {TRUE}
    | "false"   {FALSE}
    | "if"      {IF}
    | "then"    {THEN}
    | "else"    {ELSE}
    | "String"  {STRING}
    | "Int"     {INT}
    | "Bool"    {BOOL}
    | "Unit"    {UNIT}
    | "()"      {UNITVAL}
    | '('       {LPAREN}
    | ')'       {RPAREN}
    | "="       {EQ}
    | "->"      {ARROW}
    | "."       {DOT}
    | "&&"      {AND}
    | "||"      {OR}
    | "<"       {LT}
    | ">"       {GT}
    | ">="      {GEQ}
    | "<="      {LEQ}
    | "=="      {EQQ}
    | "!="      {NEQ}
    | "+"       {PLUS}
    | "-"       {MINUS}
    | "*"       {STAR}
    | "/"       {DIVIDE}
    | ","       {COMMA}
    | eof       {EOF}
    | ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']* 
        {IDENTIFIER (lexeme lexbuf)}
    (* | '"' {(* Read the string somehow...*)} *)
    | ['0'-'9']+ as num_string {INTVAL (int_of_string num_string)}
    | _         { raise (lexical_error ("Illegal character: " ^ lexeme lexbuf))}
    