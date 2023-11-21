%{
    open Slambda.Constructors.TypeConstructors
    open Slambda.Constructors.ExprConstructors
    open Slambda
    open Slambda.Types
    open Hashtbl

    let table = create 1024;;
%}

%start expr_main
%type <Types.Expr.t> expr_main

%start typ_main
%type <Types.Type.t> typ_main


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

// Every possible type.
typ:
    | base_typ ARROW typ { makeTypeFunc $1 $3}
    | base_typ           { $1 }

// Type primatives.
base_typ:
    | LPAREN typ RPAREN {$2}
    | STRING            {makeString}
    | INT               {makeInt}
    | BOOL              {makeBool}

// Complex expressions.
expr:
    | LAMBDA IDENTIFIER DOT expr     {makeExprFunc $2 None $4}
    | LET IDENTIFIER EQ expr IN expr {makeLet $2 $4 $6}
    | IF expr THEN expr ELSE expr    {makeIf $2 $4 $6}
    | value base_expr                {makeApplic $1 $2}
    | base_expr {$1}

// Fundamental expressions.
base_expr:
    | base_expr AND base_expr      {makeOpBinary OpBinary.And $1 $3}
    | base_expr OR base_expr       {makeOpBinary OpBinary.Or $1 $3}
    | base_expr EQQ base_expr      {makeOpBinary OpBinary.Equal $1 $3}
    | base_expr NEQ base_expr      {makeOpBinary OpBinary.NotEqual $1 $3}
    | base_expr LT base_expr       {makeOpBinary OpBinary.Less $1 $3}
    | base_expr GT base_expr       {makeOpBinary OpBinary.Greater $1 $3}
    | base_expr LEQ base_expr      {makeOpBinary OpBinary.LessEqual $1 $3}
    | base_expr GEQ base_expr      {makeOpBinary OpBinary.GreaterEqual $1 $3}
    | base_expr PLUS base_expr     {makeOpBinary OpBinary.Add $1 $3}
    | base_expr MINUS base_expr    {makeOpBinary OpBinary.Subtract $1 $3}
    | base_expr STAR base_expr     {makeOpBinary OpBinary.Multiply $1 $3}
    | base_expr DIVIDE base_expr   {makeOpBinary OpBinary.Divide $1 $3}
    | value {$1}

// Base values.
value :
    | LPAREN expr RPAREN {$2}
    | IDENTIFIER         {try find table $1 with Not_found -> makeVar ($1)}
    | STRINGVAL          {makeConst (Types.Constant.ConstString $1)}
    | INTVAL             {makeConst (Types.Constant.ConstInt $1)}
    | TRUE               {makeConst (Types.Constant.ConstBool true)}
    | FALSE              {makeConst (Types.Constant.ConstBool false)}

// Entry point.
expr_main:
    | expr EOF {$1}
typ_main:
    | typ EOF {$1}