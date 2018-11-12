open Types

let level_to_string(level) = match level with
    | Low -> "L"
    | High -> "H"
    | Unknown -> "U"

(* Converts an expression to a string *)
let rec expr_to_string e =
  match e with
  | Identifier(s) -> s
  | Number(i) -> string_of_int i
  | Operation(e1, BinaryOperator(op), e2) ->
      begin
        match op with
        | Plus -> (expr_to_string e1) ^ " + " ^ (expr_to_string e2)
        | Minus -> (expr_to_string e1) ^ " - " ^ (expr_to_string e2)
        | Multiplication -> (expr_to_string e1) ^ " * " ^ (expr_to_string e2)
        | Division -> (expr_to_string e1) ^ " / " ^ (expr_to_string e2)
        | Equal -> (expr_to_string e1) ^ " = " ^ (expr_to_string e2)
        | NotEqual -> (expr_to_string e1) ^ " != " ^ (expr_to_string e2)
        | LessThan -> (expr_to_string e1) ^ " < " ^ (expr_to_string e2)
        | LessThanOrEqual -> (expr_to_string e1) ^ " <= " ^ (expr_to_string e2)
        | GreaterThan -> (expr_to_string e1) ^ " > " ^ (expr_to_string e2)
        | GreaterThanOrEqual -> (expr_to_string e1) ^ " >= " ^ (expr_to_string e2)
      end
      
let value_type_to_string(vt) =
  let data_type_to_string(dt) = 
    match dt with
    | Integer -> "int"
  in 
  match vt with
  | Value(dt) -> 
      data_type_to_string(dt)
  | Channel(l, dt) ->
      "("^data_type_to_string(dt)^level_to_string(l)^" chan)"; 