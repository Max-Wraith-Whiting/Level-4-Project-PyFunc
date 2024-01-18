%{
    open Hashtbl
    open Ast.Constant
    open Ast.Constructor
    open Ast.OpBinary

    let var_table = create 1024;;
    (*let fn_table = create 1024;;*)
%}

%start main
%type <Ast.Expr.tree> main

// Tokens
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
%token AND OR
%token LT GT GEQ LEQ EQQ NEQ
%token PLUS MINUS
%token STAR DIVIDE
%token COMMA
%token COLON
// %token FIRST SECOND
%token DEFINE
// %token LIST // On a probationary status.
%token EOF
%token <string> ID
%token <string> STRINGVAL
%token <int> INTVAL

// Precedence
%left AND OR
%left LT GT GEQ LEQ EQQ NEQ
%left PLUS MINUS
%left STAR DIVIDE

%%

expr:
    | IF expr COLON expr ELSE expr              {makeIf $2 $4 $6}
    // | value base_expr                        {}
    // | DEFINE ID LPAREN param RPAREN COLON expr  {try find fn_table $2 with Not_found -> makeFunc $2 $4 $7}// Define a function.
    // | value LPAREN args RPAREN                  {makeCall $1 $3}    // Call a function
    | base_expr                                 {$1}

param:
    | value {makeParam $1}

args:
    | value {makeArgs $1}

base_expr:
    | base_expr AND base_expr    {makeOpBinary And $1 $3}
    | base_expr OR base_expr     {makeOpBinary Or $1 $3}
    | base_expr EQQ base_expr    {makeOpBinary Equal $1 $3}
    | base_expr NEQ base_expr    {makeOpBinary NotEqual $1 $3}
    | base_expr LT base_expr     {makeOpBinary Less $1 $3}
    | base_expr GT base_expr     {makeOpBinary Greater $1 $3}
    | base_expr LEQ base_expr    {makeOpBinary LessEqual $1 $3}
    | base_expr GEQ base_expr    {makeOpBinary GreaterEqual $1 $3}
    | base_expr PLUS base_expr   {makeOpBinary Add $1 $3}
    | base_expr MINUS base_expr  {makeOpBinary Subtract $1 $3}
    | base_expr STAR base_expr   {makeOpBinary Multiply $1 $3}
    | base_expr DIVIDE base_expr {makeOpBinary Divide $1 $3} 
    | value                      {$1}

value:
    // | LPAREN expr COMMA expr RPAREN
    | LPAREN expr RPAREN {$2}
    | ID                 {try find var_table $1 with Not_found -> makeVar ($1)}
    | STRINGVAL          {makeConst (ConstString $1)}
    | INTVAL             {makeConst (ConstInt $1)}
    | TRUE               {makeConst (ConstBool true)}
    | FALSE              {makeConst (ConstBool false)}
    | UNITVAL            {makeConst ConstUnit}

main:
    | expr EOF {$1}