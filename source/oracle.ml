open Types
open ListUtils

let smallerThanOrEqual(lvl1, lvl2) =
  match (lvl1, lvl2) with
  | (High, High) -> true
  | (High, Low) -> false
  | (High, Unknown) -> false
  | (Low, High) -> true
  | (Low, Low) -> true
  | (Low, Unknown) -> true
  | (Unknown, High) -> true
  | (Unknown, Low) -> false
  | (Unknown, Unknown) -> true

(* Returns the highest security level between lvl1 and lvl2 *)
let sup(lvl1, lvl2) = 
  if smallerThanOrEqual(lvl1, lvl2) then
    lvl2
  else
    lvl1

(* Returns the supremum between the variables t1 and t2 *)
let supVariables(v1, v2) = match v1, v2 with
  | Variable(Value(Integer), l1), Variable(Value(Integer), l2) -> Variable(Value(Integer), sup(l1, l2))
  | Variable(Channel(cl1, Integer), l1), Variable(Channel(cl2, Integer), l2) ->
    if (cl1 <> cl2) then
      Variable(Channel(Unknown, Integer), sup(l1, l2))
    else
      Variable(Channel(cl1, Integer), sup(l1, l2))
  | _ -> Bottom


(* Returns the value associated to the variable x in the environment g *)
let get(x, g) = 
  try
    List.assoc x g
  with
  | Not_found -> failwith ("Error : Variable "^x^" is undefined.")

(* Returns the type of the expression e in the environment g *)
let rec inferE(g, e) =
  match e with
  | Operation(e1,_,e2) -> supVariables(inferE(g, e1), inferE(g, e2))
  | Number(_) -> Variable(Value(Integer), Low)
  | Identifier(x) -> get(x, g)


(***
 * Description : Returns a list of all the variables present in the expression e 
 * Parameters :
 *   e : Expression
 *   a : Accumulator
 ***)            
let rec getListOfVariablesInExpression(e, a) = 
  match e with
  | Operation(e1,_,e2) -> union(getListOfVariablesInExpression(e1, a), getListOfVariablesInExpression(e2, a)) 
  | Number(_) -> a
  | Identifier(x) -> union(a, [Identifier(x)])

(***
 * Description : Returns the list of all Assign commands present in c 
 * Parameters :
 *   c : Command
 *   a : Accumulator
 ***)
let rec getListOfAssign(c, a) =
  match c with
  | Sequence(c1, c2) -> union(getListOfAssign(c1, a), getListOfAssign(c2, a))
  | IfThenElse(_, c1, c2) -> union(getListOfAssign(c1, a), getListOfAssign(c2, a))
  | WhileDo(e, c1) -> union(a, getListOfAssign(c1, a))
  | Assign(e1, e2) -> union(a, [Assign(e1, e2)])
  | _ -> a

let rec getListOfWhileDo(c, a) =
  match c with
  | Sequence(c1, c2) -> union(getListOfWhileDo(c1, a), getListOfWhileDo(c2, a))
  | IfThenElse(_, c1, c2) -> union(getListOfWhileDo(c1, a), getListOfWhileDo(c2, a))
  | WhileDo(e, c1) -> union(a, getListOfWhileDo(c1, a))
  | Assign(e1, e2) -> union(a, [Assign(e1, e2)])
  | _ -> a


(***
 * Description : Determines if the variable x is incremented in this assignation
 * Parameters :
 *   x : Variables identifier
 *   a : Assign command
 ***)
let variableIsIncrementedByConstant(x, a) =
  match a with
  | Assign(id, e) when id = x ->
      begin
        match e with
        | Operation(x, BinaryOperator(Plus), Number(_)) -> true
        | _ -> false
      end
  | _ -> false


(***
 * Description : Determines if the variable x is decremented in this assignation
 * Parameters :
 *   x : Variables identifier
 *   a : Assign command
 ***)
let variableIsDecrementedByConstant(x, a) =
  match a with
  | Assign(id, e) when id = x ->
      begin
        match e with
        | Operation(x, BinaryOperator(Minus), Number(_)) -> true
        | _ -> false
      end
  | _ -> false

let rec variableValueIsChanged(x, c) =
  match c with
  | Sequence(c1, c2) 
  | IfThenElse(_, c1, c2) -> variableValueIsChanged(x, c1) || variableValueIsChanged(x, c2)
  | WhileDo(_, c1) -> variableValueIsChanged(x, c1)
  | Assign(id, _) 
  | Receive(id, _) -> if id = x then true else false
  | Send(_,_) -> false 
  | Skip -> false

let isHigh(Identifier(x), g) =
  let v = get(x, g) in
  match v with
  | Variable(_, High) -> true
  | _ -> false

(***
 * Description : 
 * Parameters :  
 *   p : Command to analyze
 *   g : Environment
 ***)
let basicOracle(p, g, pc, tl) = 
  match p with
  | WhileDo(e, c) ->
      begin
        match e with
        | Number(i) when i > 0 -> (Diverge, Low)      (* Equivalent to "while true do". Our language does not have a break command, hence it will always diverge. *)
        | Number(i) when i <= 0 -> (Terminate, Low)  (* Equivalent to "while false do". It will never be executed. *)
        | _ ->
            let Variable(_, l) = inferE(g, e) in
              (Maybe, l)
            
      end      
  | _ -> (Maybe, Unknown)

let oracle(p, g, pc, tl) = basicOracle(p, g, pc, tl)

let print_termination_level (t, l) = 
  match t with
  | Terminate -> print_string ("T"^Convert.level_to_string(l))
  | Diverge -> print_string ("D")
  | Maybe -> print_string ("M"^Convert.level_to_string(l)) 





let testEnvironment = [("lowChannel", Variable(Channel(Low, Integer), Low));
                       ("highChannel", Variable(Channel(High, Integer), Low));
                       ("highValue", Variable(Value(Integer), High));
                       ("lowValue", Variable(Value(Integer), Low));
                       ("x", Variable(Value(Integer), Low))]

let testCaseWhileTrue =
  let p = WhileDo(Number(1), Skip) in
  oracle(p, testEnvironment, Low, (Terminate, Low))

let testCaseWhileFalse =
  let p = WhileDo(Number(0), Skip) in
  oracle(p, testEnvironment, Low, (Terminate, Low))
  
let testCaseWhileWithIncrementedVariable =
  let p = WhileDo(Operation(Identifier("x"), BinaryOperator(LessThan), Number(100)), Assign(Identifier("x"), (Operation(Identifier("x"), BinaryOperator(Plus), Number(1))))) in
  oracle(p, testEnvironment, Low, (Terminate, Low))  
  
let testCaseWhileWithVariablesThatAreNotModified =
  let p = WhileDo(Identifier("x"), Skip) in
  oracle(p, testEnvironment, Low, (Terminate, Low))  