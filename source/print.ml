open Printf
open Types
open Convert
		
let rec print_expression e =
	match e with
	| Identifier(s) -> printf "Identifier(%s)" s
	| Number(i) -> printf "Number(%d)" i
	| Operation(e1, op, e2) -> 
			printf "Operator("; 
			print_expression e1;
			printf ", ";
			print_expression e2;
			printf ")" 

let rec print_command c = 
	match c with
	| Assign(e1, e2) ->
			printf "Assign("; 
			print_expression e1;
			printf ", ";
			print_expression e2;
			printf ")" 
	| Skip -> printf "Skip"
	| IfThenElse(e, c1, c2) ->
			printf "IfThenElse("; 
			print_expression e;
			printf ", ";
			print_command c1;
			printf ", ";
			print_command c2;
			printf ")"
	| WhileDo(e, c) ->
			printf "WhileDo("; 
			print_expression e;
			printf ", ";
			print_command c;
			printf ")"
	| Sequence(c1, c2) -> 
			printf "Sequence("; 
			print_command c1;
			printf ", ";
			print_command c2;
			printf ")"
	| Receive(e1, e2) ->
			printf "Receive("; 
			print_expression e1;
			printf ", ";
			print_expression e2;
			printf ")" 
	| Send(e1, e2) ->
			printf "Send("; 
			print_expression e1;
			printf ", ";
			print_expression e2;
			printf ")"

let print_level_letter(l) = match l with
	| Low -> printf "L"
	| Unknown -> printf "U"
	| High -> printf "H"

let print_environment(g) =
	let print_data_type dt = match dt with
		| Integer -> printf "int"
	in 
	let print_variable (id, var) = match var with
		| Variable(Value(dt),l) -> 
			print_string ("\t" ^ id);
			print_string (" = (");
			print_data_type(dt);
			print_string ")";
			print_level_letter(l);
			print_string "\n"
		| Variable(Channel(cl, dt), l) ->
			print_string ("\t" ^ id);
			print_string (" = (");
			print_data_type(dt);
			print_level_letter(cl);
			print_string " chan)";
			print_level_letter(l);
			print_string "\n"
		| Level(l) -> 
			print_string ("\t" ^ id);
			print_string (" = ");
			print_level_letter(l); 
			print_string "\n"
		| TerminationLevel((t, l)) ->
			print_string ("\t" ^ id);
			print_string (" = ");
			begin match t with
			| Terminate -> print_string "T"; print_level_letter(l)
			| Diverge -> print_string "D"
			| Maybe -> print_string "M"; print_level_letter(l);
			end;
			print_string "\n"
		| Boolean(b) -> 
			print_string ("\t" ^ id);
			print_string (" = ");
			print_string ((string_of_bool(b))^"\n");
		| Bottom -> 
			print_string ("\t" ^ id^" = Bottom\n");
	in
	List.iter (fun x -> print_variable x) g#to_list
	
let print_header s =
		print_string ("\n\n\n" ^ s ^ "\n===================================================\n")
		