{
(* code CAML *)

open Lexing
open Parser
   
exception Lexing_error of string;;
    
let keyword_table = [ 
"if",IF;
"then",THEN;
"else",ELSE;
"let",LET;
"and",ANDLET;
"in",IN;
"true",TRUE;
"false",FALSE; 
"function",FUNCTION;
"fun",FUN;
"rec",REC;
"not",NOT;
"match",MATCH; 
"with",WITH
]
;;
  
 (* voir l'alternative proposé dans le cours (p 52) avec Hashtbl comme dans mini-pascal *)
let ident_or_keyword s = try List.assoc s keyword_table with _ -> IDENT s;; (* _ car il n'y a qu'une seule exception*)
  
(* ici with précise quels champs on modifie par rapport à pos dans l'enregistrment fournit*)
(* lex_curr_p = position courante du pointeur de fichier*)
let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

(* FIN code CAML *)
}

(* les types utilisé ci-dessous en majuscule sont définis par le parser *)

let digit 		= ['0'-'9']
let letter 		= ['a'-'z' 'A'-'Z']
let ident 		= ['a'-'z'] (letter | '_' | '\'' | digit)*
let integer 	= ['0'-'9']+
let space 		= [' ' '\t']
let chaine		= '"'([^ '\\' '"' '\n'] | '\\''"' | '\\''\\' | '\\''n')* '"'

rule token = parse
  | '\n'    			{ newline lexbuf; token lexbuf } (* sur la pile de reccursion *)
  | space+  			{ token lexbuf }
  | ident as id 		{ ident_or_keyword id }
  | '+'     			{ PLUS }
  | '-'     			{ MINUS }
  | '*'     			{ TIMES }
  | '/'     			{ DIV }
  | '='     			{ EQ }
  | "<>"    			{ NEQ }
  | '<'     			{ LT }
  | "<="    			{ LE }
  | '>'     			{ GT }
  | ">="    			{ GE }
  | '('     			{ LPAREN }
  | ')'     			{ RPAREN }
  | '['					{ LCROCH }
  | ']'					{ RCROCH }
  | '_'					{ JOCKER }
  | ','     			{ COMMA }
  | '|'					{ PIPE }
  | "->"				{ ARROW }
  | "::"    			{ DOUBLECOLON }
  | "&&"    			{ AND }
  | "||"    			{ OR }
  | ';'     			{ SEMICOLON }
  | ";;"    			{ DOUBLESEMICOLON }  
  | "(*"    			{ comment lexbuf; token lexbuf } (* sur la pile de reccursion *)
  | chaine	as s		{ STRING s }
  | integer as s 		{ INTEGER (int_of_string s) }
  | eof     			{ EOF }
  | _ as c  			{ raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

and comment = parse
  | "*)"    			{ () }
  | "(*"				{ comment lexbuf; comment lexbuf } (* sur la pile de reccursion *)
  | '\n'				{ newline lexbuf; comment lexbuf }
  | _       			{ comment lexbuf }
  | eof     			{ raise (Lexing_error ("unterminated comment")) }
