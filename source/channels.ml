open Types

module Channels = Map.Make(String);;
let channels = ref Channels.empty;;

let exists(name) = Channels.exists (fun key value -> key = name) channels.contents;;	

let add_channel(name, level) =
	channels := Channels.add name (Variable(Channel(level, Integer), Low)) channels.contents;;

let add_public_channel(name) =
	add_channel(name, Low);;

let add_private_channel(name) =
	add_channel(name, High);;

let get_list_of_channels() =
	Channels.fold (fun key chan list -> (key, chan)::list) channels.contents [];;

let get_list_of_names() =
	Channels.fold (fun key _ list -> key::list) channels.contents [];;