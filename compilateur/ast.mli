(*syntaxe abstraite*)

type ident = Ident 	of string * (Lexing.position * Lexing.position)
;;

type const =
	| True 			of (Lexing.position * Lexing.position)
	| False 		of (Lexing.position * Lexing.position)
	| Entier 		of int * (Lexing.position * Lexing.position)
	| Chaine 		of string * (Lexing.position * Lexing.position)
	| Nothing 		of (Lexing.position * Lexing.position)
;;

type unop =
	| Uminus 		of (Lexing.position * Lexing.position)
	| Not 			of (Lexing.position * Lexing.position)
;;


type binop =
	| Add 			of (Lexing.position * Lexing.position)
	| Sub 			of (Lexing.position * Lexing.position) 
	| Mul 			of (Lexing.position * Lexing.position) 
	| Div 			of (Lexing.position * Lexing.position) 
	| Le 			of (Lexing.position * Lexing.position) 
	| Ge 			of (Lexing.position * Lexing.position) 
	| Lt 			of (Lexing.position * Lexing.position) 
	| Gt 			of (Lexing.position * Lexing.position) 
	| Neq 			of (Lexing.position * Lexing.position) 
	| Eq 			of (Lexing.position * Lexing.position) 
	| And 			of (Lexing.position * Lexing.position) 
	| Or 			of (Lexing.position * Lexing.position) 
;;

type motif =
	| Jocker 		of (Lexing.position * Lexing.position) 
	| Var 			of ident
	| Assoc 		of motif * motif * motif list * (Lexing.position * Lexing.position) 
;;

type expr =
	| Expression 	of simple_expr
	| Application 	of simple_expr * simple_expr * (Lexing.position * Lexing.position)
	| Unop 			of unop * expr * (Lexing.position * Lexing.position)
	| Binop 		of expr * binop * expr * (Lexing.position * Lexing.position)
	| Ifthenelse 	of expr * expr * expr * (Lexing.position * Lexing.position)
	| Empile 		of expr * expr * (Lexing.position * Lexing.position)
	| Function 		of motif * expr * (Lexing.position * Lexing.position)
	| Letin 		of motif * expr * expr * (Lexing.position * Lexing.position)
	| Letrecin 		of motif * expr * expr * (Lexing.position * Lexing.position)
	| Matchwith 	of expr * expr * motif * motif * expr * (Lexing.position * Lexing.position)

and simple_expr =
	| Bracket 		of expr * (Lexing.position * Lexing.position)
	| Called 		of ident
	| Const 		of const
	| Arg 			of expr * expr * expr list * (Lexing.position * Lexing.position)
	| Liste 		of (Lexing.position * Lexing.position)
;;

type decl =
	| Let 			of motif * expr * (Lexing.position * Lexing.position)
	| Letrec 		of motif * expr * (Lexing.position * Lexing.position)
;;

type fichier = decl list
;;
