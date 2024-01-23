%{
    open Hashtbl
    open Py_ast.Constant
    open Py_ast.Constructor
    open Py_ast.OpBinary

    let var_table = create 1024;;
%}

%start start
%type <Py_ast.Expr.tree> start

// Tokens
%token TRUE
%token FALSE
%token IF
// %token THEN
%token ELSE
%token UNITVAL
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
// %token EQ
%token AND OR
%token LT GT GEQ LEQ EQQ NEQ
%token PLUS MINUS
%token STAR DIVIDE
%token COMMA
%token COLON
%token DEFINE
%token EOF
// %token NL
%token <string> ID
%token <string> STRINGVAL
%token <int> INTVAL
// %token LIST // On a probationary status.

// Precedence
%left AND OR
%left LT GT GEQ LEQ EQQ NEQ
%left PLUS MINUS
%left STAR DIVIDE

%%

expr:
    | IF base_expr COLON scope ELSE scope       {makeIf $2 $4 $6}
    // | value LPAREN RPAREN                    {makeCall $1 (makeConst ConstUnit)}       // Call without params.
    | ID LPAREN arg_list RPAREN                 {makeCall $1 $3}    // Call with params.
    | DEFINE ID LPAREN param_list RPAREN scope  {makeFunc $2 $4 $6} // Define a function.
    | base_expr                                 {$1}

scope: LBRACE expr RBRACE   {$2}

param_list:
    | xs = separated_nonempty_list(COMMA, ID) { xs }

arg_list:
    | xs = separated_nonempty_list(COMMA, value) { xs }


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
    | LPAREN expr RPAREN {$2}
    | ID                 {try find var_table $1 with Not_found -> makeVar ($1)}
    | STRINGVAL          {makeConst (ConstString $1)}
    | INTVAL             {makeConst (ConstInt $1)}
    | TRUE               {makeConst (ConstBool true)}
    | FALSE              {makeConst (ConstBool false)}
    | UNITVAL            {makeConst ConstUnit}

start:
    | expr EOF {$1}