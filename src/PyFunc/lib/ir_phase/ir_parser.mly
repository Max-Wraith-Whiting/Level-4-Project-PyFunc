%{
    open HM.Constructors.TypeConstructors
    open HM.Constructors.ExprConstructors
    open HM
    open HM.Ast
    open Hashtbl

    let table = create 1024;;
%}

%start expr_main
%type <Ast.Expr.tree> expr_main

%start typ_main
%type <Ast.Type.t> typ_main


// Tokens
%token LAMBDA
%token LET
%token REC
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
%token LBRACK
%token RBRACK
%token EQ
%token ARROW
%token DOT
%token AND OR
%token NOT
%token LT GT GEQ LEQ EQQ NEQ
%token PLUS MINUS
%token STAR DIVIDE
%token CONS
%token COMMA
%token FIRST SECOND
%token EOF
%token <string> ID
%token <string> STRINGVAL
%token <int> INTVAL

// Precedence
%right NOT
%right UPLUS UMINUS
%left LT GT GEQ LEQ EQQ NEQ
%left PLUS MINUS
%left STAR DIVIDE
%left AND OR

%%

// Every possible type.
typ:
    | base_typ ARROW typ { makeTypeFunc $1 $3}
    | base_typ STAR typ  { makeTypePair $1 $3}
    | base_typ           { $1 }

// // Type primatives.
base_typ:
    | LPAREN typ RPAREN {$2}
    | STRING            {makeString}
    | INT               {makeInt}
    | BOOL              {makeBool}
    | UNIT              {makeUnit}

// // Complex expressions.
expr:
    | LAMBDA ID DOT expr                            {makeFunc $2 $4}
    | LET REC ID EQ expr IN expr                    {makeLetRec $3 $5 $7}
    | LET ID EQ expr IN expr                        {makeLet $2 $4 $6}
    | LET LPAREN ID COMMA ID RPAREN EQ expr IN expr {makeLetPair $3 $5 $8 $10}
    | FIRST expr                                    {makeFirst $2}
    | SECOND expr                                   {makeSecond $2}
    | IF expr THEN expr ELSE expr                   {makeIf $2 $4 $6}
    | value LPAREN base_expr RPAREN                 {makeApplic $1 $3}
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
    | base_expr CONS base_expr     {makeOpBinary OpBinary.Cons $1 $3}
    | unary_op {$1}

unary_op:
    | PLUS  base_expr %prec UPLUS {makeOpUnary OpUnary.Positive $2}
    | MINUS e = base_expr %prec UMINUS {makeOpUnary OpUnary.Negative e}
    | NOT   base_expr {makeOpUnary OpUnary.Not $2}
    | value {$1}

// Base values.
value:
    | LPAREN expr COMMA expr RPAREN {makePair $2 $4}
    | LPAREN expr RPAREN            {$2}
    | LBRACK value_list RBRACK      {makeList $2}
    | ID                            {try find table $1 with Not_found -> makeVar ($1)}
    | STRINGVAL                     {makeConst (Ast.Constant.ConstString $1)}
    | INTVAL                        {makeConst (Ast.Constant.ConstInt $1)}
    | TRUE                          {makeConst (Ast.Constant.ConstBool true)}
    | FALSE                         {makeConst (Ast.Constant.ConstBool false)}
    | UNITVAL                       {makeConst Ast.Constant.ConstUnit}

value_list:
    | xs = separated_list(COMMA, value) {xs}

// expr:
//     | LET ID EQ expr IN expr                        {makeLet $2 $4 $6}
//     | LET LPAREN ID COMMA ID RPAREN EQ expr IN expr {makeLetPair $3 $5 $8 $10}
//     | IF expr THEN expr ELSE expr                   {makeIf $2 $4 $6}
//     | LPAREN expr RPAREN                            {$2}
//     | LAMBDA ID DOT expr                            {makeFunc $2 $4}
//     | base_expr {$1}

// logic_or:
//     | logic_and (OR logic_and)* {makeOpBinary OpBinary.Or $1 $3}
//     | logic_and {$1}

// logic_and:
//     | equality AND equality {makeOpBinary OpBinary.And $1 $3}
//     | equality {$1}

// equality:
//     | comparison NEQ comparison {makeOpBinary OpBinary.NotEqual $1 $3}
//     | comparison EQQ comparison {makeOpBinary OpBinary.Equal $1 $3}
//     | comparison {$1}

// comparison:
//     | term LT term  {makeOpBinary OpBinary.Less $1 $3}
//     | term LEQ term {makeOpBinary OpBinary.LessEqual $1 $3}
//     | term GT term  {makeOpBinary OpBinary.Greater $1 $3}
//     | term GEQ term {makeOpBinary OpBinary.GreaterEqual $1 $3}
//     | term {$1}

// term:
//     | factor MINUS factor {makeOpBinary OpBinary.Subtract $1 $3}
//     | factor PLUS factor  {makeOpBinary OpBinary.Add $1 $3}
//     | factor {$1}

// factor:
//     | unary DIVIDE unary {makeOpBinary OpBinary.Divide $1 $3}
//     | unary STAR unary   {makeOpBinary OpBinary.Multiply $1 $3}
//     | unary {$1}

// unary:
//     | NOT unary   {makeOpUnary OpUnary.Not $2}
//     | MINUS unary {makeOpUnary OpUnary.Negative $2}
//     | PLUS unary  {makeOpUnary OpUnary.Positive $2}
//     | list_op     {$1}

// list_op:
//     | unary CONS unary {makeOpBinary OpBinary.Cons $1 $3}
//     | call             {$1}

// call:
//     | primary LPAREN expr RPAREN {makeApplic $1 $3}
//     | sub_call {$1}

// sub_call:
//     | LET REC ID EQ expr IN expr {makeLetRec $3 $5 $7}
//     | FIRST expr                 {makeFirst $2}
//     | SECOND expr                {makeSecond $2}
//     | primary {$1}

// value_list:
//     | xs = separated_list(COMMA, expr) {xs}

// primary:
//     | LPAREN expr COMMA expr RPAREN {makePair $2 $4}
//     | LBRACK value_list RBRACK      {makeList $2}
//     | ID                            {try find table $1 with Not_found -> makeVar ($1)}
//     | STRINGVAL                     {makeConst (Ast.Constant.ConstString $1)}
//     | INTVAL                        {makeConst (Ast.Constant.ConstInt $1)}
//     | TRUE                          {makeConst (Ast.Constant.ConstBool true)}
//     | FALSE                         {makeConst (Ast.Constant.ConstBool false)}
//     | UNITVAL                       {makeConst Ast.Constant.ConstUnit}

// Entry point.
expr_main:
    | expr EOF {$1}
typ_main:
    | typ EOF {$1}