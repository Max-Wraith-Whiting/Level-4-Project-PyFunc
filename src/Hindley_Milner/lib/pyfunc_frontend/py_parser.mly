%{
    open Hashtbl
    open Py_ast.Constructor
    open Py_ast

    let var_table = create 1024;;
      
%}

%start start
%type <Py_ast.Expr.tree> start

// Tokens
%token TRUE
%token FALSE
%token IF
%token ELIF
// %token THEN
%token ELSE
%token UNITVAL
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACK
%token RBRACK
%token EQ
%token AND OR
%token LT GT GEQ LEQ EQQ NEQ
%token PLUS MINUS
%token STAR DIVIDE INTDIVIDE
%token EXPONENT
%token NOT
%token CONS
%token HEAD
%token TAIL
%token MOD
%token COMMA
%token COLON
%token DEFINE
%token EOF

%token <string> ID
%token <string> STRINGVAL
%token <int> INTVAL
%token <float> FLOATVAL
// %token LIST // On a probationary status.

// Precedence
%right CONS NOT
%right HEAD TAIL
%right UPLUS UMINUS
%left LT GT GEQ LEQ EQQ NEQ
%left PLUS MINUS
%left STAR DIVIDE INTDIVIDE
%right EXPONENT
%left AND OR

%%
start:
    | binding+ EOF {makeProgram $1}

binding:
    | DEFINE ID LPAREN param_list RPAREN COLON scope {makeBinding $2 (makeFunc $2 $4 $7)}
    | ID EQ expr {makeBinding $1 $3}

scope: LBRACE expr RBRACE   {$2}

expr:
    // | DEFINE ID LPAREN param_list RPAREN scope expr  {update_binding_list(makeFunc $2 $4 $6); $7}
    | base_expr                                 {$1}
    | if_expr                                   {$1}

    // | ID EQ value {makeAssign $1 $3 } // Requires a heap scope.

base_expr:
    | expr OR expr        {makeOpBinary OpBinary.And $1 $3}
    | expr AND expr       {makeOpBinary OpBinary.Or $1 $3}
    | expr NEQ expr       {makeOpBinary OpBinary.NotEqual $1 $3}
    | expr EQQ expr       {makeOpBinary OpBinary.Equal $1 $3}
    | expr LT expr        {makeOpBinary OpBinary.Less $1 $3}
    | expr LEQ expr       {makeOpBinary OpBinary.LessEqual $1 $3}
    | expr GT expr        {makeOpBinary OpBinary.Greater $1 $3}
    | expr GEQ expr       {makeOpBinary OpBinary.GreaterEqual $1 $3}
    | expr MINUS expr     {makeOpBinary OpBinary.Subtract $1 $3}
    | expr PLUS expr      {makeOpBinary OpBinary.Add $1 $3}
    | expr DIVIDE expr    {makeOpBinary OpBinary.Divide $1 $3}
    | expr STAR expr      {makeOpBinary OpBinary.Multiply $1 $3}
    | expr MOD expr       {makeOpBinary OpBinary.Mod $1 $3}
    | expr INTDIVIDE expr {makeOpBinary OpBinary.IntDivide $1 $3}
    | expr EXPONENT expr  {makeOpBinary OpBinary.Exponent $1 $3}
    | unary             {$1}
// Control Flow

if_expr:
    | IF expr COLON scope elif_expr   {makeIf $2 $4 $5}
    | IF expr COLON scope else_expr   {makeIf $2 $4 $5}

elif_expr:
    | ELIF expr COLON scope elif_expr {makeIf $2 $4 $5}
    | ELIF expr COLON scope else_expr {makeIf $2 $4 $5}

else_expr:
    | ELSE COLON scope {$3}

unary:
    | NOT unary   {makeOpUnary OpUnary.Not $2}
    | MINUS unary %prec UMINUS {makeOpUnary OpUnary.Negative $2}
    | PLUS unary  %prec UPLUS  {makeOpUnary OpUnary.Positive $2}
    | HEAD unary {makeOpUnary OpUnary.Head $2}
    | TAIL unary {makeOpUnary OpUnary.Tail $2}
    | list_op     {$1}

list_op:
    | unary CONS unary {makeOpBinary OpBinary.Cons $1 $3}
    | call {$1}

call:
    | ID LPAREN expr_list RPAREN {makeCall $1 $3}    // Call with params.
    | primary {$1}

primary:
    | LPAREN expr RPAREN {$2}
    | LBRACK expr_list RBRACK {makeList $2}
    | ID                 {try find var_table $1 with Not_found -> makeVar ($1)}
    | STRINGVAL          {makeConst (ConstString $1)}
    | INTVAL             {makeConst (ConstInt $1)}
    | FLOATVAL           {makeConst (ConstFloat $1)}
    | TRUE               {makeConst (ConstBool true)}
    | FALSE              {makeConst (ConstBool false)}
    | UNITVAL            {makeConst ConstUnit}

// List literals.

param_list:
    | xs = separated_list(COMMA, ID) { xs }

expr_list:
    | xs = separated_list(COMMA, expr) { xs }