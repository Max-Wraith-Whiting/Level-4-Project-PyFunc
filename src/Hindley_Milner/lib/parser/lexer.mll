{
    (* open Tokens *)
    open Lexing
}
(* NEED TO DEFINE parse function*)
rule token = parse
    | [' ' '\t' '\r' '\n']* (* Parse whitespace *)
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
    | ['0'-'9']+ {INTVAL (int_of_string (lexeme lexbuf))}
    