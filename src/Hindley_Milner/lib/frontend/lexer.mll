{
    open Parser
    open Lexing

    exception Lexical_error of string
    let lexical_error msg = Lexical_error msg
}

rule token = parse
    | "True"    {TRUE}
    | "False"   {FALSE}
    | "if"      {IF}
    | "else"    {ELSE}
    | "String"  {STRING}
    | "Int"     {INT}
    | "Bool"    {BOOL}
    | "Unit"    {UNIT}
    | "()"      {UNITVAL}
    | '('       {LPAREN}
    | ')'       {RPAREN}
    | '='       {EQ}
    | '.'       {DOT}
    | "and"      {AND}
    | "or"      {OR}
    | '<'       {LT}
    | '>'       {GT}
    | ">="      {GEQ}
    | "<="      {LEQ}
    | "=="      {EQQ}
    | "!="      {NEQ}
    | '+'       {PLUS}
    | '-'       {MINUS}
    | '*'       {STAR}
    | '/'       {DIVIDE}
    | ','       {COMMA}
    | ':'       {COLON}
    | eof       {EOF}