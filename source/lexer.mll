{
  open Parser (* The tokens are defined in parser.mli *)
  exception End_of_file
  exception Error of string
  
  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    };;
  
  let create_hashtable size init =
    let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl

  let keyword_table = 
    create_hashtable 16 [
      ("skip", SKIP);
      ("if", IF);
      ("then", THEN);
      ("else", ELSE);
      ("end", END);
      ("while", WHILE);
      ("do", DO);
      ("receive", RECEIVE);
      ("from", FROM);
      ("send", SEND);
      ("to", TO)
    ]
    
  let operator_table =
    create_hashtable 16 [
      ("+", PLUS);
      ("-", MINUS);
      ("*", TIMES);
      ("/", DIV);
      ("=", EQ);
      ("!=", NEQ);
      ("<", LT);
      ("<=", LTEQ);
      (">", GT);
      (">=", GTEQ);
      (":=", ASSIGN)
    ]
}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']*

rule toy_lang = parse
  | digit+ as inum
    { let num = int_of_string inum in
    INT num
  }
  | id as word
    { try
        let token = Hashtbl.find keyword_table word in
        token
      with Not_found ->
        IDENTIFIER word
    }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | "=" { EQ }
  | "!=" { NEQ }
  | "<" { LT }
  | "<=" { LTEQ }
  | ">" { GT }
  | ">=" { GTEQ }
  | ":=" { ASSIGN }

  | [' ' '\t' '\r']  { toy_lang lexbuf }
  | '\n' { incr_linenum lexbuf;  toy_lang lexbuf }
  | ';' { SEMICOLON }
  | eof  { EOF }
  | _ { raise (Error (Printf.sprintf "Unexpected character \"%s\" near offset %d.\n" (Lexing.lexeme lexbuf) (Lexing.lexeme_start lexbuf))) }