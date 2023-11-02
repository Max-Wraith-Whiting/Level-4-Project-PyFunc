// Tokens
%token LAMBDA
%token LET
%token IN
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token STRING
%token INT
%token BOOL
%token UNIT
%token UNITVAL
%token LPAREN
%token RPAREN
%token EQ
%token ARROW
%token DOT
%token AND OR
%token LT GT GEQ LEQ EQQ NEQ
%token PLUS MINUS
%token STAR DIVIDE
%token COMMA
%token EOF
%token <string> IDENTIFIER
%token <string> STRINGVAL
%token <int> INTVAL

// Precedence
%left AND OR
%left LT GT GEQ LEQ EQQ NEQ
%left PLUS MINUS
%left STAR DIVIDE

%%