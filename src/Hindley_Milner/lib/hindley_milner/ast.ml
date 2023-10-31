type op = ADD | SUB | GREATER | LESS | AND | OR

module CharacterMap = Map.Make(String)

type primitive_type =
  | Type_num
  | Type_bool
  | Type of string
  | Type_func of primitive_type * primitive_type
;;

type id = string

type expr = 
  | Literal_num of int
  | Literal_bool of bool
  | Value of string
  | Binary_op of expr * op * expr
  | Func of id * expr
  | Application of expr * expr
;;


let op_to_string (op: op): string = 
  match op with
  | ADD -> "+" 
  | SUB -> "-"
  | GREATER -> ">"
  | LESS -> "<"
  | OR -> "||"
  | AND -> "&&"


let rec expr_to_string (e: expr): string = 
  match e with
  | Literal_num x -> string_of_int x
  | Literal_bool b -> string_of_bool b
  | Value s -> s
  | Binary_op(e1, op, e2) ->
    let string_1 = expr_to_string e1
     and string_2 = expr_to_string e2
     in let op_string = op_to_string op 
    in "(" ^ string_1 ^ ", " ^ op_string ^ ", " ^ string_2 ^ ")"
  | Func(id, e) -> let string_1 = expr_to_string e in "(fun " ^ id ^ " -> " ^ string_1
  | Application(e1, e2) -> 
    let string_1 = expr_to_string e1 
     and string_2 = expr_to_string e2 
     in "(" ^ string_1 ^ ", " ^ string_2 ^ " )"
;;


let op_to_string (op: op): string = 
  match op with
  | ADD -> "+" 
  | SUB -> "-"
  | GREATER -> ">"
  | LESS -> "<"
  | OR -> "||"
  | AND -> "&&"