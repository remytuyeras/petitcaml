
open Ast

let rec simple_expr_of_exprs l pos =
	match l with
	| [] -> Liste(pos)
	| x::y -> Bracket(Empile(x,Expression(simple_expr_of_exprs y pos),pos),pos)
;;

let rec function_of_motifs motif l expr pos =
	match l with
	| [] -> Function(motif,expr,pos)
	| x::y -> Function(motif,function_of_motifs x y expr pos,pos)
;;

let rec expr_of_liste_simple_expr se l pos =
	match l with
	| [] -> Expression(se)
	| [x] -> Application(se,x,pos)
	| x::y -> expr_of_liste_simple_expr (Bracket(Application(se,x,pos),pos)) y pos
;;

let rec letand_of_list l =
	match l with
	| [] -> ([],[])
	| (xm,xe)::y -> let (ym,ye) = letand_of_list y in (xm::ym,xe::ye)
;;
