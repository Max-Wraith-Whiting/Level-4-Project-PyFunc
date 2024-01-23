{
    open Py_parser
    open Lexing

    exception Lexical_error of string
    let lexical_error msg = Lexical_error msg
}

rule token = parse
    (* Source Cleaners *)
    | [' ' '\t' '\r' '\n']* { token lexbuf }
    | '#'[^'\n']*           { token lexbuf }  (* Single line comments. *)
    (* | '\n'      {NL} Newline *)
    | "def"     {DEFINE}
    | "True"    {TRUE}
    | "False"   {FALSE}
    | "if"      {IF}
    | "else"    {ELSE}
    | "()"      {UNITVAL}
    | '('       {LPAREN}
    | ')'       {RPAREN}
    | '{'       {LBRACE}
    | '}'       {RBRACE}
    (* | '='       {EQ} *)
    (* | '.'       {DOT} *)
    | "and"     {AND}
    | "or"      {OR}
    (* | "not"     {NOT} *)
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
    | ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']*  {ID (lexeme lexbuf)}
    | '"'       {read_string (Buffer.create 17) lexbuf}
    | ['0'-'9']+ as num_string {INTVAL (int_of_string num_string)}
    | _         { raise (lexical_error ("Illegal character: " ^ lexeme lexbuf))}
    and read_string buffer = parse
        | '"'           {STRINGVAL (Buffer.contents buffer)}
        | '\\' '/'      {Buffer.add_char buffer '/'; read_string buffer lexbuf}
        | '\\' '\\'     {Buffer.add_char buffer '\\'; read_string buffer lexbuf}
        | '\\' 'b'      {Buffer.add_char buffer '\b'; read_string buffer lexbuf}
        | '\\' 'f'      {Buffer.add_char buffer '\012'; read_string buffer lexbuf}
        | '\\' 'n'      {Buffer.add_char buffer '\n'; read_string buffer lexbuf}
        | '\\' 'r'      {Buffer.add_char buffer '\r'; read_string buffer lexbuf}
        | '\\' 't'      {Buffer.add_char buffer '\t'; read_string buffer lexbuf}
        | [^ '"' '\\']+ {Buffer.add_string buffer (Lexing.lexeme lexbuf);read_string buffer lexbuf}
        | _             {raise (Lexical_error ("Illegal string character: " ^ Lexing.lexeme lexbuf))}
        | eof           {raise (Lexical_error ("String is not terminated"))}