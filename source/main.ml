open Types
open Environment
open Printf

let filename = ref ""
let usage = "usage: " ^ Sys.argv.(0) ^ " -file filename [-publicchannels string] [-privatechannels string]"
let speclist =
	[("-file", Arg.String (fun s -> filename := s), "The file containing the code to analyze.");
	 ("-publicchannels", Arg.String (fun s -> List.iter (fun s -> Channels.add_public_channel(s)) (Str.split (Str.regexp ",") s)), "The names of the different public channels (separated by commas).");
	 ("-privatechannels", Arg.String (fun s -> List.iter (fun s -> Channels.add_private_channel(s)) (Str.split (Str.regexp ",") s)), "The names of the different private channels (separated by commas).");
   
	]

let _ =
	let command = ref Skip in
	begin
  try
			Arg.parse speclist (fun x -> raise (Arg.Bad ("Bad argument : " ^ x))) usage;
   			begin
						let cin = open_in filename.contents in
              let lexbuf = Lexing.from_channel cin in
								try
                  	command := Parser.instructions Lexer.toy_lang lexbuf;
            				Print.print_header "Syntax tree returned by the parser";
            				Print.print_command command.contents;
            				Print.print_header "Inference";
            				let _ = Analyzer.infer(new environment(), 1, Low, command.contents) in ();

										Print.print_header "Instrumentation";
										(* We then instrument the code (even if it is statically safe). *)
             				let _ = Analyzer.instrument(command.contents, 1) in ()
               			(*print_string code.contents*)
								with
								| Lexer.Error message ->
										printf  "Error (Lexer) : %s" message
								| Parsing.Parse_error ->
										let currentPosition = lexbuf.Lexing.lex_curr_p in
                    let line = currentPosition.Lexing.pos_lnum in
                    let column = currentPosition.Lexing.pos_cnum - currentPosition.Lexing.pos_bol in
                    let token = Lexing.lexeme lexbuf in
										failwith (sprintf "Error (Parsing) : A problem occured near the token \"%s\" (line %d, column %d)." token line column)
				end
		with
		| Failure explanation -> print_string (explanation)
		| Lexer.End_of_file -> exit 0
		end;