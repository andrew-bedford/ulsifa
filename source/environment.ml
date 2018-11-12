open Types

let defaultInitialContent = [("lowChannel", Variable(Channel(Low, Integer), Low));
														("highChannel", Variable(Channel(High, Integer), Low));
													  ("highValue", Variable(Value(Integer), High));
													  ("lowValue", Variable(Value(Integer), Low))];;

class environment 
	?init:(initialContent=defaultInitialContent) () =
	let environment = Hashtbl.create 0 in
	object(self)
		method domain = Hashtbl.fold (fun variable value accumulator -> variable::accumulator) environment []
		method image = Hashtbl.fold (fun variable value accumulator -> value::accumulator) environment []
		method get(variable) = Hashtbl.find environment variable
		method remove(variable) =	Hashtbl.remove environment variable
		method update(variable, value) = Hashtbl.replace environment variable value
		method copy = new environment ~init:(self#to_list) ()
		method to_list = Hashtbl.fold (fun variable value accumulator -> (variable, value)::accumulator) environment []
		method is_empty = (Hashtbl.length(environment) = 0)
		method includes_bottom = List.mem Bottom self#image
				
		initializer
				List.iter (fun (variable, value) -> self#update(variable, value)) initialContent
	end;;