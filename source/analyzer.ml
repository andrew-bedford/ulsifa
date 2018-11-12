open Types
open List
open ListUtils
open Printf
open Environments
open Environment


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
 
let weakSmallerThanOrEqual(lvl1, lvl2) =
	match (lvl1, lvl2) with
	| (High, Low) -> false
	| _ -> true

(* Returns the lowest security level between lvl1 and lvl2 *)
let inf(lvl1, lvl2) = 
	if smallerThanOrEqual(lvl1, lvl2) then
		lvl1
	else
		lvl2

(* Returns the supremum (highest security level) between lvl1 and lvl2 *)
let sup(lvl1, lvl2) = 
	if smallerThanOrEqual(lvl1, lvl2) then
		lvl2
	else
		lvl1

(* Returns the infimum (lowest security level) between lvl1 and lvl2 *)
let inf(lvl1, lvl2) = 
	if smallerThanOrEqual(lvl1, lvl2) then
		lvl1
	else
		lvl2

let isChannel(var) = 
	match var with
	| Variable(Channel(_, _),_) -> true
	| _ -> false

let isValue(var) =
	match var with
	| Variable(Value(_), _) -> true
	| _ -> false
 

(* Returns the supremum between the variables v1 and v2 *)
let supVariables(v1, v2) = match v1, v2 with
	| Variable(Value(Integer), l1), Variable(Value(Integer), l2) -> Variable(Value(Integer), sup(l1, l2))
	| Variable(Channel(cl1, Integer), l1), Variable(Channel(cl2, Integer), l2) ->
		if (cl1 <> cl2) then
			Variable(Channel(Unknown, Integer), sup(l1, l2))
		else
			Variable(Channel(cl1, Integer), sup(l1, l2))
	| Level(l1), Level(l2) -> Level(sup(l1, l2))
	| Boolean(b1), Boolean(b2) -> Boolean(b1 || b2)
	| TerminationLevel(tl1), TerminationLevel(tl2) -> TerminationLevel(tl1) (* ignore *)
	| _ -> Bottom

let supTerminationLevels((t1, l1), (t2, l2)) = 
	if t1 = t2 then begin
		(t1, sup(l1, l2))
	end
	else if t1 = Diverge || t2 = Diverge then begin
		(Diverge, Low)
	end
	else begin
		(Maybe, sup(l1, l2))
	end

let oplus(l, ((t1, l1) as tl1), ((t2, l2) as tl2)) =  
	if tl1 = tl2 && 
		(tl1 != (Maybe, Low) || l = Low) then begin
		tl1
	end
	else if (t1 = Terminate && t2 = Terminate) then begin
		(Terminate, sup(l1, l2))
	end
	else if (l = Low && t1 != t2 &&	
					is_member(tl1, [(Terminate,Low); (Diverge,Low); (Maybe,Low)]) &&
					is_member(tl2, [(Terminate,Low); (Diverge,Low); (Maybe,Low)])) then begin
		(Maybe, Low)
	end
	else if (l = High && (
						is_member((Maybe,High), union([tl1],[tl2])) ||
						(is_member(t1, [Terminate; Diverge]) && is_member(t2, [Terminate; Diverge]))
					)) then begin
		(Maybe, High)
	end
	else begin
		(Maybe, Unknown)
	end

(* Returns the domain (identifiers) of the environment g *)
let dom(g) = map (fun (x,_) -> x) g

(* Returns the value associated to the variable x in the environment g *)
let get(x, g) = 
	try
		assoc x g
	with
	| Not_found -> failwith ("Error : Variable "^x^" is undefined.")

(** Returns the supremum of the environments g1 and g2 *)
let supEnv(ga, gb) =
	let g1 = ga#to_list in
	let g2 = gb#to_list in
	if is_empty(g1) then gb
	else
		if is_empty(g2) then ga
		else begin
  			let g3 = Hashtbl.create 0 in 
  				List.iter (fun (key, value) -> Hashtbl.replace g3 key value) (union(g1, g2));
  				List.iter (fun x ->
    				if (is_member(x, diff(dom(g1), dom(g2)))) then
    					Hashtbl.replace g3 x (get(x, g1))
    				else if (is_member(x, diff(dom(g2), dom(g1)))) then
    					Hashtbl.replace g3 x (get(x, g2))
    				else
    					Hashtbl.replace g3 x (supVariables(get(x, g2), get(x, g1)))
					) (dom(union(g1, g2)));
					let result = (Hashtbl.fold (fun key value acc -> (key, value) :: acc) g3 []) in
  				new environment ~init:result ()
		end

(* Checks if one of the variables of g has Bottom as security type. This indicates *)
(* that there has been an error. *)
let incBottomEnv(g) =
	List.exists (fun x -> (get(x, g) = Bottom)) (dom(g))

(* Checks if the environements g1 and g2 are equal *)
let eqEnv(g1, g2) =
	if (dom(g1) = dom(g2)) then begin
			List.for_all (fun x -> (get(x,g1) = get(x,g2))) (dom(g1))
	end
	else
		false

(* Returns the type of the expression e in the environment g *)
let rec inferE(g, e) =
	match e with
	| Operation(e1,_,e2) -> supVariables(inferE(g, e1), inferE(g, e2))
	| Number(_) -> Variable(Value(Integer), Low)
	| Identifier(x) -> get(x, g)


(* Verifies that an identifier is not reserved (or a constant). *)
let verify_identifier_is_not_reserved(identifier) =
	if identifier = "pc" ||
		 identifier = "highChannel" ||
		 identifier = "lowChannel" ||
		 identifier = "highValue" ||
		 identifier = "lowValue"
	then
		failwith "Error : The following names are reserved : pc, highChannel, lowChannel, highValue, lowValue. You cannot assign them a value."
	else
		()

(* g contains the environments at different points in the program (g(1), g(2), etc.) *)
let g = new environments;;
let initialEnvironment = new environment();;
g#update(0, initialEnvironment);;


let result_message = ref (0, "OK");;

(* Infers the security level of the command cmd given the context pc *)
let rec infer((ge:environment), i, pc, cmd) =
	match cmd with
	| Skip ->
			printf "\n\n"; Print.print_command Skip; printf "\n";
			
			let terminationLevel = (Terminate, Low) in			
			let gi = ge#copy in
			gi#update("_pc", Level(pc));
			gi#update("_tl", TerminationLevel(terminationLevel));
			g#update(i, gi);
			
			Oracle.print_termination_level(terminationLevel);
			printf "\ng(%d) :\n" i;
			Print.print_environment(g#get(i));
			(terminationLevel, g#get(i), i+1)
			
	| Assign(Identifier(x), e) ->
			printf "\n\n"; Print.print_command (Assign(Identifier(x), e)); printf "\n";
			verify_identifier_is_not_reserved(x);
			begin
  			let Variable(vt, l) = inferE(ge#to_list, e) in
				
				let terminationLevel = (Terminate, Low) in
				
				let gi = ge#copy in
			  gi#update(x, Variable(vt, sup(pc, l)));
				gi#update("_pc", Level(pc));
				gi#update("_tl", TerminationLevel(terminationLevel));
				g#update(i, gi);
				
				Oracle.print_termination_level(terminationLevel);
				printf "\ng(%d) :\n" i;
				Print.print_environment(g#get(i));
				(terminationLevel, g#get(i), i+1)
			end
			
	| Sequence(c1, c2) ->
			printf "\n\n"; Print.print_command (Sequence(c1, c2));  printf "\n";
			let (((t1,l1) as tl1), g1, j) = infer(ge, i, pc, c1) in
			
			if t1 = Diverge then begin
				let terminationLevel = (Diverge, Low) in
				(terminationLevel, g1, j)
			end
			else begin
				let (((t2,l2) as tl2), g2, k) = infer(g1, j, sup(pc,l1), c2) in
				let terminationLevel = supTerminationLevels(tl1, tl2) in
				(terminationLevel, g2, k)
			end
				
	| Receive(Identifier(x1), Identifier(x2)) ->
			printf "\n\n"; Print.print_command (Receive(Identifier(x1), Identifier(x2))); printf "\n";
			verify_identifier_is_not_reserved(x1);
			begin
				match ge#get(x2) with
				| Variable(Channel(cl, dt), l) ->
						let terminationLevel = (Terminate, Low) in
						
						let gi = ge#copy in
					  gi#update(x1, Variable(Value(dt), (sup(sup(pc, cl),l))));
						gi#update(x2, Variable(Channel(cl, dt), sup(pc, l)));
						gi#update("_pc", Level(pc));
						gi#update("_tl", TerminationLevel(terminationLevel));
						g#update(i, gi);
						
						Oracle.print_termination_level(terminationLevel);
						printf "\ng(%d) :\n" i;
						Print.print_environment(g#get(i));
						(terminationLevel, g#get(i), i+1)
				| _ ->
						let error_message = ("Error (Receive) : "^x2^" must be a channel.") in
						result_message := (i, error_message);
						failwith error_message
			end
	
	| Send(Identifier(x1), Identifier(x2)) ->
			printf "\n\n"; Print.print_command (Send(Identifier(x1), Identifier(x2))); printf "\n";
			if isValue(ge#get(x1)) && isChannel(ge#get(x2)) then begin
				let Variable(Value(dt), l1) = ge#get(x1) in
				let Variable(Channel(cl, dt), l2) = ge#get(x2) in
				
				let gi = ge#copy in
				gi#update("_pc", Level(pc));
				
				if cl = High then begin
					let terminationLevel = (Terminate, Low) in
					gi#update("_tl", TerminationLevel(terminationLevel));
					g#update(i, gi);
					Oracle.print_termination_level(terminationLevel);
					printf "\ng(%d) :\n" i;
					Print.print_environment(gi);
					(terminationLevel, gi, i+1)
				end
				else begin
					if weakSmallerThanOrEqual(sup(sup(pc, l1), l2),  cl) && cl != High then begin
  					let terminationLevel = (Terminate, inf(pc, Unknown)) in
						gi#update("_tl", TerminationLevel(terminationLevel));
						g#update(i, gi);
						Oracle.print_termination_level(terminationLevel);
						printf "\ng(%d) :\n" i;
						Print.print_environment(gi);
						(terminationLevel, gi, i+1)
  				end
					else begin
						let error_message = ("Error (Send) : Cannot send "^x1^" ("^(Convert.level_to_string l1)^") to "^x2^" ("^(Convert.level_to_string cl)^") in context "^(Convert.level_to_string pc)^".") in
  					result_message := (i, error_message);
  					failwith error_message
					end
				end;					
				
				
			end
			else begin
				let error_message = ("Error (Send) : "^x1^" must be a value and "^x2^" must be a channel.") in
				result_message := (i, error_message);
				failwith error_message
			end
	
	| IfThenElse(e, c1, c2) ->
			printf "\n\n"; Print.print_command (IfThenElse(e, c1, c2)); printf "\n";
			if isValue(inferE(ge#to_list, e)) then begin
				let Variable(_, l) = inferE(ge#to_list, e) in
				let pc_if = sup(pc, l) in
				let (tl1, g1, j) = infer(ge, i+1, pc_if, c1) in
				let (tl2, g2, k) = infer(ge, j, pc_if, c2) in
				if not ((supEnv(g1, g2))#includes_bottom) then begin
					g#update(i, (new environment ~init:((supEnv(g1, g2))#to_list)()));
					let terminationLevel = oplus(l, tl1, tl2) in
					
					let gi = g#get(i) in
					gi#update("_pc", Level(pc));
					gi#update("_tl", TerminationLevel(terminationLevel));
					gi#update("_tlOfBranch1", TerminationLevel(tl1));
					gi#update("_tlOfBranch2", TerminationLevel(tl2));
					g#update(i, gi);
					
					Oracle.print_termination_level(terminationLevel);
					printf "\ng(%d) :\n" i;
					Print.print_environment(g#get(i));
					(terminationLevel, g#get(i), k)
				end
				else begin
					let error_message = ("Error (IfThenElse) : Variables must have the same nature (value, channel) in the \"then\" and \"else\" branches.") in
					result_message := (i, error_message);
					failwith error_message
				end
			end
			else begin
				let error_message = ("Error (IfThenElse) : The condition must be evaluated to a value.") in
				result_message := (i, error_message);
				failwith error_message
			end
	
	| WhileDo(e, c) ->
			printf "\n\n"; Print.print_command (WhileDo(e, c)); printf "\n";
  		if isValue(inferE(ge#to_list, e)) then begin
				let Variable(_, l) = inferE(ge#to_list, e) in
				let pc_while = sup(pc, l) in
				let (((tp, lp) as tlp), gep, j) = infer(ge, i+1, pc_while, c) in
				let jr = ref j in
				let tlr = ref tlp in
				if eqEnv(ge#to_list, (supEnv(ge, gep))#to_list) && not (incBottomEnv((supEnv(ge, gep))#to_list)) then begin
	    		g#update(i, (new environment ~init:((supEnv(ge, gep))#to_list)()))
	  		end
	  		else if not (eqEnv(ge#to_list, (supEnv(ge, gep))#to_list)) && not ((supEnv(ge, gep))#includes_bottom) then begin
	  			let (tlres, gres, j) = (infer(supEnv(ge, gep), i, sup(pc_while, lp), WhileDo(e, c))) in
					jr := j;
					tlr := tlres;
	  		end;

				let terminationLevel = Oracle.oracle(WhileDo(e, c), ge#to_list, pc, tlr.contents) in
				
				let gi = g#get(i) in
				gi#update("_pc", Level(pc));
				gi#update("_tl", TerminationLevel(terminationLevel));
				g#update(i, gi);
				
				Oracle.print_termination_level(terminationLevel);
				printf "\ng(%d) :\n" i;
	  		Print.print_environment(g#get(i));
	  		(terminationLevel, g#get(i), jr.contents)
			end
			else begin
				let error_message = ("Error (IfThenElse) : The condition must be evaluated to a value.") in
				result_message := (i, error_message);
				failwith error_message
			end

	| _ ->
		let error_message = "Error (Semantic error)" in
		result_message := (i, error_message);
		failwith error_message


(* String that will contain the instrumented code *)
let code = ref "pc := L;\n"

(* Helper functions to add indentation levels to the instrumented code *)
let indentation_level = ref 0
let increase_indentation_level() =
	indentation_level := indentation_level.contents + 1
let decrease_indentation_level() =
	indentation_level := indentation_level.contents - 1
let add_indentation() =
	String.make indentation_level.contents '\t'

let rec level_of_expression_string(g, e) = 
	match e with
	 Operation(e1,_,e2) ->
		begin
			let s1 = level_of_expression_string(g, e1) in
			let s2 = level_of_expression_string(g, e2) in
			match (s1, s2) with
			| "H", _ -> "H"
			| _, "H" -> "H"
			| "L", "L" -> "L"
			| "L", r -> r
			| l, "L" -> l
			| r, l -> r^" sup "^l
		end
	| Number(_) -> "L"
	| Identifier(x) -> 
			begin
  			match g#get(x) with
  			| Variable(Channel(contentLevel, Integer), containerLevel) ->
						begin
  						match contentLevel, containerLevel with
							| High, _ -> "H"
							| _, High -> "H"
							| Unknown, Unknown -> x^"_cl sup "^x^"_l"
							| Unknown, _ -> x^"_cl"
							| _, Unknown -> x^"_l"
							| Low, Low -> "L"
						end
  			| Variable(Value(Integer), containerLevel) ->
					begin
						match containerLevel with
						| High -> "H"
						| Unknown -> x^"_l"
						| Low -> "L"
					end
				| Level(l) -> Convert.level_to_string(l)
				| TerminationLevel(tt, l) ->
						if l = Unknown then begin
							"terminationLevelOfCommand<i>"
						end
						else begin
							Convert.level_to_string(l)
						end
				| _ -> failwith "Error (level_of_expression_string)."
			end
			
(* Generates the instrumented code *)
let rec instrument(command, i) =
	let add_line(line) = code := code.contents ^ add_indentation() ^ line ^ "\n" in
	match command with
	| Skip ->
			add_line("skip");
			i+1
			
	| Assign(Identifier(x), e) ->
			begin
  			match g#get_variable(i, x) with
  			| Variable(Channel(contentLevel, Integer), containerLevel) ->
  					add_line(x^"_cl := "^"channelLevelOf("^(Convert.expr_to_string e)^")");
  					add_line(x^"_l := "^"pc sup levelOf("^(Convert.expr_to_string e)^")") 
  			| Variable(Value(Integer), containerLevel) ->
  					add_line(x^"_l := "^"pc sup "^level_of_expression_string(g#get(i), e)^"")
				| _ -> failwith "Error (instrument->assign)" 
			end;
			add_line(x ^ " := " ^ (Convert.expr_to_string e) ^ ";");
			i+1
			
	| Receive(Identifier(x1), (Identifier(x2) as e)) ->
			add_line(x1^"_l := pc sup "^level_of_expression_string(g#get(i), e)^";");
			add_line("recv "^x1^" from "^x2^";");
			i+1
			
	| Sequence(c1, c2) ->
			let j = instrument(c1, i) in
			add_line("pc := <G(i)(pc) sup levelOf(G(i)(tt))>);");
			let k = instrument(c2, j) in
			k
	
	| Send(Identifier(x1), Identifier(x2)) ->		
			if (g#get_variable(i, "_instr") = Boolean(true)) then begin
				add_line("guardedSend "^x1^" to "^x2^";")
			end
			else begin
				add_line("send "^x1^" to "^x2^";");
			end;
			i+1

	| IfThenElse(e, c1, c2) ->
			let terminationType = g#get_variable(i, "_tl") in
			add_line("pcBeforeCommand"^string_of_int(i)^" := pc;");
			add_line("if "^(Convert.expr_to_string e)^" then");
			increase_indentation_level();
			add_line("pc := <G(i)(pc) sup  "^level_of_expression_string(g#get(i), e)^";");
			let j = instrument(c1, i+1) in
			decrease_indentation_level();
			add_line("else");
			increase_indentation_level();
			add_line("pc := <G(i)(pc) sup levelOf("^(Convert.expr_to_string e)^")>;");
			let k = instrument(c2, j) in
			decrease_indentation_level();
			add_line("end;");
			add_line("pc := pcBeforeCommand"^string_of_int(i)^";");
 			k
		
	| WhileDo(e, c) ->
			let terminationType = g#get_variable(i, "_tl") in
      add_line("pcBeforeCommand"^string_of_int(i)^" := pc;");
      add_line("while "^(Convert.expr_to_string e)^" do");
			increase_indentation_level();     
      add_line("pc := pc sup <levelOf("^(Convert.expr_to_string e)^")>;");
      let j = instrument(c, i+1) in
			decrease_indentation_level();
      add_line("end;");
      add_line("pc := pcBeforeCommand"^string_of_int(i)^";");
      j
			
	| _ -> failwith "Error (instrument) : Unknown error"



