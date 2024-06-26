open Ast.Expr
open Ast.Type

module ExprConstructors = struct
  let makeConst const = ExprConst const
  let makeVar variable = ExprVar variable
  let makeLet binder expr_a expr_b = ExprLet (binder, expr_a, expr_b)
  let makeOpUnary op expr = ExprOpUnary (op, expr)
  let makeOpBinary op expr_a expr_b = ExprOpBinary (op, expr_a, expr_b)
  let makeFunc binder body = ExprFunc (binder, body)
  let makeLetRec binder expr_a expr_b = ExprLetRec (binder, expr_a, expr_b)
  let makeApplic expr_a expr_b = ExprApplic (expr_a, expr_b)
  let makeIf condition expr_a expr_b = ExprIf (condition, expr_a, expr_b)
  let makeLetPair quant_a quant_b expr_a expr_b = ExprLetPair (quant_a, quant_b, expr_a, expr_b)
  let makePair expr_a expr_b = ExprPair (expr_a, expr_b)
  let makeFirst expr = ExprFirst expr
  let makeSecond expr = ExprSecond expr
  let makeList list = ExprList list
end

module TypeConstructors = struct
  let makeBool = TypeBool
  let makeInt = TypeInt
  let makeString = TypeString
  let makeUnit = TypeUnit
  let makeTypeFunc type_a type_b = TypeFunc (type_a, type_b)
  let makeTypePair type_a type_b = TypePair (type_a, type_b)
end