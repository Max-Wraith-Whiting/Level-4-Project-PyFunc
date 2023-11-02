%{
    open Lambda
%}

%start expr_main
%type <Lambda_sig.Expr.t> expr_main

%start typ_main
%type <Lambda_sig.Type.t> typ_main

%%

// Every possible type.
typ:
    | base_typ ARROW typ { Lambda_sig.TypeConstructors ($1 $3)}
    | base_typ           { $1 }

// Type primatives.
base_typ:
    | LPAREN typ RPAREN {$2}
    | STRING            {TypeConstructors.make_string}
    | INT               {TypeConstructors.make_int}
    | BOOL              {TypeConstructors.make_bool}

// Complex expressions.
expr:
    | LAMBDA IDENTIFIER DOT expr     {ExprConstructors.make_func $2 None $4}
    | LET IDENTIFIER EQ expr IN expr {ExprConstructors.make_let $2 $4 $6}
    | IF expr THEN expr ELSE expr    {ExprConstructors.make_if $2 $4 $6}
    | value base_expr                 {ExprConstructors.make_application $1 $2}
    | base_expr {$1}

// Fundamental expressions.
base_expr:
    | base_expr AND base_expr      {ExprConstructors.make_op_binary OpBinary.And $1 $3}
    | base_expr OR base_expr       {ExprConstructors.make_op_binary OpBinary.Or $1 $3}
    | base_expr EQQ base_expr      {ExprConstructors.make_op_binary OpBinary.Equal $1 $3}
    | base_expr NEQ base_expr      {ExprConstructors.make_op_binary OpBinary.NotEqual $1 $3}
    | base_expr LT base_expr       {ExprConstructors.make_op_binary OpBinary.Less $1 $3}
    | base_expr GT base_expr       {ExprConstructors.make_op_binary OpBinary.Greater $1 $3}
    | base_expr LEQ base_expr      {ExprConstructors.make_op_binary OpBinary.LessEqual $1 $3}
    | base_expr GEQ base_expr      {ExprConstructors.make_op_binary OpBinary.GreaterEqual $1 $3}
    | base_expr PLUS base_expr      {ExprConstructors.make_op_binary OpBinary.Add $1 $3}
    | base_expr MINUS base_expr {ExprConstructors.make_op_binary OpBinary.Subtract $1 $3}
    | base_expr STAR base_expr     {ExprConstructors.make_op_binary OpBinary.Multiply $1 $3}
    | base_expr DIVIDE base_expr   {ExprConstructors.make_op_binary OpBinary.Divide $1 $3}
    | value {$1}

// Base values.
value :
    | LPAREN expr RPAREN {$2}
    | IDENTIFIER         {(*FIND IDENTIFIER FROM TABLE(?)*)}
    | STRINGVAL          {ExprConstructors.make_string Constant.ConstString $1}
    | INTVAL             {ExprConstructors.make_int Constant.ConstInt $1}
    | TRUE               {ExprConstructors.make_bool Constant.ConstBool true}
    | FALSE              {ExprConstructors.make_bool Constant.ConstBool false}

// Entry point.
expr_main:
    | expr EOF {$1}
typ_main:
    | typ EOF {$1}