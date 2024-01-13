open Types.Expr
open Types.Type

module ExprConstructors = struct
  let makeConst const = ExprConst const
  let makeVar variable = ExprVar variable
  let makeLet binder expr_a expr_b = ExprLet (binder, expr_a, expr_b)
  let makeOpBinary op expr_a expr_b = ExprOpBinary (op, expr_a, expr_b)
  let makeExprFunc binder typ body = 
    match typ with
      | Some typ -> ExprFunc(binder, typ, body)
      | None -> raise (Errors.parse_error "All lambdas must be annotated.")
  let makeApplic expr_a expr_b = ExprApplic (expr_a, expr_b)
  let makeAnn expr typ = ExprAnn (expr, typ)
  let makeIf condition expr_a expr_b = ExprIf (condition, expr_a, expr_b)
end

module TypeConstructors = struct
  let makeBool = TypeBool
  let makeInt = TypeInt
  let makeString = TypeString
  let makeUnit = TypeUnit
  let makeTypeFunc type_a type_b = TypeFunc (type_a, type_b)
end