%{
    open Hashtbl

    let table = create 1024;;
%}

%start main
%type <Ast.Expr.tree> main