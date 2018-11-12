type program = 
	| Program of instruction list 
and instruction = 
	| Command of command
and expression =
	| Identifier of string
	| Number of int
	| Operation of expression * binaryOperator * expression
and command =
	| Assign of expression * expression
	| Skip
	| IfThenElse of expression * command * command
	| WhileDo of expression * command
	| Sequence of command * command
	| Receive of expression * expression
	| Send of expression * expression
and binaryOperator =
	| BinaryOperator of binaryOperators
and binaryOperators =
	| Plus
	| Minus
	| Multiplication
	| Division
	| Equal
	| NotEqual
	| LessThan
	| LessThanOrEqual
	| GreaterThan
	| GreaterThanOrEqual
and level =
	| High 
	| Unknown
	| Low
and dataType =
	| Integer
and valueType =
	| Value of dataType
	| Channel of level * dataType
and terminationType = 
	| Terminate
	| Diverge
	| Maybe
and variableType =
	| Variable of valueType * level
	| Level of level
	| Boolean of bool
	| TerminationLevel of (terminationType * level)
	| Bottom