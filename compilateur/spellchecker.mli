type cident = CIdent of string ;;

type cconst =
	| CEntier of int 			
	| CAsciiz of int * string 		
;;

type cunop = CUminus | CNot ;;

type cbinop =
	| CAdd | CSub | CMul | CDiv 
	| CLe | CGe | CLt | CGt  
	| CNeq | CEq 
	| CAnd | COr 
;;

type cmotif = 
	| CJocker 
	| CVar 			of cident
	| CAssoc		of cmotif * cmotif * cmotif list
;;

type cexpr =
	| CExpression 	of csimple_expr
	| CApplication 	of csimple_expr * csimple_expr
	| CUnop 		of cunop * cexpr
	| CBinop 		of cexpr * cbinop * cexpr
	| CIfthenelse 	of cexpr * cexpr * cexpr
	| CEmpile 		of cexpr * cexpr							
	| CClos	 		of int * cident list						
	| CLetandin 	of (cmotif * cexpr) list * cexpr 
	| CLetrecin 	of cmotif * cexpr * cexpr						

and csimple_expr =
	| CPrint_int
	| CPrint_string
	| CPrint_newline
	| CRead_int
	| CRead_line
	| CBracket 		of cexpr
	| CCalled 		of cident
	| CConst 		of cconst
	| CArg 			of cexpr * cexpr * cexpr list					
;;

type cdecl =
	| CLetand		of (cmotif * cexpr) list
	| CLetrec		of cmotif * cexpr					 							
	| CLetfun		of int * cident list * cmotif * cexpr 
;;

type cfichier = cdecl list ;;

module Sset :
	sig
		type elt = String.t
		type t = Set.Make(String).t
		val empty : t
		val is_empty : t -> bool
		val mem : elt -> t -> bool
		val add : elt -> t -> t
		val singleton : elt -> t
		val remove : elt -> t -> t
		val union : t -> t -> t
		val inter : t -> t -> t
		val diff : t -> t -> t
		val compare : t -> t -> int
		val equal : t -> t -> bool
		val subset : t -> t -> bool
		val iter : (elt -> unit) -> t -> unit
		val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
		val for_all : (elt -> bool) -> t -> bool
		val exists : (elt -> bool) -> t -> bool
		val filter : (elt -> bool) -> t -> t
		val partition : (elt -> bool) -> t -> t * t
		val cardinal : t -> int
		val elements : t -> elt list
		val min_elt : t -> elt
		val max_elt : t -> elt
		val choose : t -> elt
		val split : elt -> t -> t * bool * t
	end;;

val assoc'asciiz : (string * int) list ref;;

val lvars'cexpr : cexpr -> Sset.t;;
val correction'cletfun : Ast.fichier -> cfichier;;
val affiche'cfichier : cfichier -> unit;;
exception Correction_echec ;;

