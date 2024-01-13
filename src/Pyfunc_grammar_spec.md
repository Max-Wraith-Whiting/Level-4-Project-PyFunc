# Pyfunc Full Grammar

## String -> Tokens
Here is the collections of tokens and their associated string to be parsed.

base_value = 
    | "True"          ->      TRUE;
    | "False"         ->      FALSE;
    | ['0'-'9']+      ->      INT;
    | ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']* -> STRING;
    | (['0'-'9']+)?"."['0'-'9'] -> FLOAT;

call = base_value ( "(" arguments? ")" || "." ID)*; // Possibly ignore . notation.

unary = ("!" || "-") unary | call;

factor = unary ( ("/" || "*" ) unary ) *;

term = factor ( ("-" || "+" ) factor) *;

comparison = term ( (">" || ">=" || "<" || "<=") term) *;

equality = comparison ( ( "!=" || "==" ) comparison)* ;

logic_and = equality ( "and" equality) *;

logic_or = logic_and ("or" logic_and) *;

assignment = (call ".")? ID "=" assignment || logic_or;

expression = assignment;

function = ID "(" parameters? ")" block;

parameters = ID ( "," ID )* ;

arguments = expression ( "," expression )* ;

if = "if" expression ":" expression "else" ":" expression

declaration = 
    | func_decl
    | var_decl

fun_decl = "def" function;

var_decl = ID ( "=" expression);

program = declaration* EOF;