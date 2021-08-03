open Ast

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

(*----------------------------------------------------------------------------*)

module Sset = Set.Make(String);;

let rec vars'list f l  =
	match l with
	| [] -> Sset.empty
	| x::y -> Sset.union (f x) (vars'list f y)
;;

let vars'ident = function Ident(s,_) -> Sset.singleton s ;;

let rec vars'motif motif =
	match motif with
	| Jocker(_) -> Sset.empty
	| Var(ident) -> vars'ident ident
	| Assoc(m1,m2,lm,_) ->  vars'list vars'motif ([m1;m2]@lm)
;;

let rec vars'simple_expr f simple_expr =
		match simple_expr with
		| Bracket(expr,_) -> f expr
		| Called(ident) -> vars'ident ident
		| Arg(expr1,expr2,list_expr,_) -> vars'list f ([expr1;expr2]@list_expr)
		| _ -> Sset.empty
;;

let rec fvars'expr expr =
	match expr with
	| Expression (simple_expr1) ->
		vars'simple_expr fvars'expr simple_expr1		
	| Application(simple_expr1,simple_expr2,_) ->
		Sset.union (vars'simple_expr fvars'expr simple_expr1) (vars'simple_expr fvars'expr simple_expr2)
	| Function(motif1,expr1,_) ->
		Sset.diff (fvars'expr expr1) (vars'motif motif1)
	| Unop(unop,expr1,_) ->
		fvars'expr expr1	
	| Binop(expr1,binop,expr2,_) ->
		Sset.union (fvars'expr expr1) (fvars'expr expr2)
	| Empile(expr1,expr2,_) ->
		Sset.union (fvars'expr expr1) (fvars'expr expr2)
	| Ifthenelse(expr1,expr2,expr3,_) ->
		vars'list fvars'expr (expr1::expr2::expr3::[])
	| Letin(motif1,expr1,expr2,_) ->
		Sset.union (fvars'expr expr1) (Sset.diff (fvars'expr expr2) (vars'motif motif1))
	| Letrecin(motif1,expr1,expr2,_) ->
		Sset.diff (Sset.union (fvars'expr expr1) (fvars'expr expr2)) (vars'motif  motif1)
	| Matchwith(expr1,expr2,motif1,motif2,expr3,_) ->
		Sset.union (fvars'expr expr2) (Sset.diff (fvars'expr expr3) (Sset.union (vars'motif motif1) (vars'motif motif2)))
;;

(*----------------------------------------------------------------------------*)

let rec correction'list f lst =
		match lst with
		| [] -> []
		| x::y -> (f x)::(correction'list f y)
;; 

let correction'cident = function Ident(s,_) ->  CIdent(s);;

let cpt'asciiz = ref 0;;
let assoc'asciiz = ref [];;

let correction'cconst const =
	match const with
	| True(_) -> CEntier(1)
	| False(_) -> CEntier(0)
	| Nothing(_) -> CEntier(0)
	| Entier(i,_) -> CEntier(i)
	| Chaine(s,_) ->
		try 
			let i = List.assoc s !assoc'asciiz in
			CAsciiz(i,s)
		with Not_found ->
			begin
			let a = CAsciiz(!cpt'asciiz,s) in
			assoc'asciiz:=(s,!cpt'asciiz)::(!assoc'asciiz);
			incr cpt'asciiz;
			a
			end			
;;

let correction'cunop unop = 
	match unop with
	| Uminus(pos) -> CUminus
	| Not(pos) -> CNot
;;

let correction'cbinop binop =
	match binop with
	| Add(_) -> CAdd
	| Sub(_) -> CSub
	| Mul(_) -> CMul 
	| Div(_) -> CDiv 
	| Le(_) -> CLe
	| Ge(_) -> CGe
	| Lt(_) -> CLt
	| Gt(_) -> CGt
	| Neq(_) -> CNeq
	| Eq(_) -> CEq
	| And(_) -> CAnd
	| Or(_) -> COr
;;

let rec correction'cmotif motif  =
	match motif with
	| Jocker(_) -> 
		CJocker
	| Var(ident) ->
		CVar(correction'cident ident)
	| Assoc(m1,m2,lm,_) ->
		let cm1 = correction'cmotif m1 in
		let cm2 = correction'cmotif m2 in
		let clm = correction'list correction'cmotif lm in
		CAssoc(cm1,cm2,clm)
;;

exception Correction_echec ;;

let rec list_letand cm ce = 
	match (cm,ce) with
	| (CJocker,_) -> [(cm,ce)]
	| (CVar(_),_) -> [(cm,ce)]
	| (CAssoc(cm1,cm2,clm),CExpression(CArg(ce1,ce2,cle))) -> 
		let rec llist_letand clm cle =
			match (clm,cle) with
			|([],[]) -> []
			|(xm::ym,xe::ye) -> (list_letand xm xe)@(llist_letand ym ye)
			| _ -> raise Correction_echec 
		in
		(list_letand cm1 ce1) @ (list_letand cm2 ce2) @ (llist_letand clm cle)
	| _ -> [(cm,ce)]
;;

let convert_called ident =
	match ident with
	| Ident("print_int",_) -> CPrint_int
	| Ident("print_string",_) -> CPrint_string
	| Ident("print_newline",_) -> CPrint_newline
	| Ident("read_int",_) -> CRead_int
	| Ident("read_line",_) -> CRead_line
	| _ -> CCalled(correction'cident ident)
;;

let numclos = ref 1;;
let list_letfun = ref [];;
let flag_rec = ref 0;;

let rec correction'cexpr expr =
	let rec correction'simple_cexpr simple_expr =
		match simple_expr with
		| Bracket(expr,_) -> 
			CBracket(correction'cexpr expr)
		| Called(ident) -> 
			convert_called ident
		| Const(const) -> 
			CConst(correction'cconst const)
		| Arg(expr1,expr2,list_expr,_) -> 
			CArg(correction'cexpr expr1,correction'cexpr expr2,correction'list correction'cexpr list_expr)
		| Liste(_) -> CConst(CEntier 0)
		in
		match expr with
		| Expression (simple_expr) -> 
			CExpression(correction'simple_cexpr simple_expr)	
		| Application(simple_expr1,simple_expr2,_) -> 
			CApplication(correction'simple_cexpr simple_expr1,correction'simple_cexpr simple_expr2)
		| Function(motif,expr,_) as funexpr ->
			if !flag_rec = 0
			then
				begin
					let lci = Sset.fold (fun s l -> (CIdent s)::l) (fvars'expr funexpr) [] in
					let cm = correction'cmotif motif in
					let ce = correction'cexpr expr in
					let i = !numclos in
					incr numclos;
					list_letfun:=(CLetfun(i,lci,cm,ce)::(!list_letfun));
					CClos(i,lci)
				end
			else 
				begin
					flag_rec:=0;
					correction'cexpr expr 
				end
		| Unop(unop,expr,pos) -> 
			CUnop(correction'cunop unop,correction'cexpr expr)
		| Binop(expr1,binop,expr2,pos) -> 
			CBinop(correction'cexpr expr1,correction'cbinop binop,correction'cexpr expr2)
		| Empile(expr1,expr2,pos) -> 
			CEmpile(correction'cexpr expr1,correction'cexpr expr2)
		| Ifthenelse(expr1,expr2,expr3,pos) -> 
			CIfthenelse(correction'cexpr expr1,correction'cexpr expr2,correction'cexpr expr3)
		| Letin(motif,expr1,expr2,pos) -> 
			CLetandin(list_letand (correction'cmotif motif) (correction'cexpr expr1),correction'cexpr expr2)
		| Letrecin(motif,expr1,expr2,pos) ->
			flag_rec:=1;
			CLetrecin(correction'cmotif motif,correction'cexpr expr1,correction'cexpr expr2)
		| Matchwith(expr1,expr2,motif1,motif2,expr3,pos) ->
			let cexpr1 = correction'cexpr expr1 in 
			let cexpr3 = CLetandin([(CAssoc(correction'cmotif motif1,correction'cmotif motif2,[]),cexpr1)],correction'cexpr expr3) in
			CIfthenelse(cexpr1,cexpr3,correction'cexpr expr2)
;;

let correction'cdecl decl =
	match decl with
	| Let(motif,expr,pos) ->
		CLetand(list_letand (correction'cmotif motif) (correction'cexpr expr))
	| Letrec(motif,expr,pos) ->
		flag_rec:=1;
		CLetrec(correction'cmotif motif,correction'cexpr expr)
;;

let rec correction'cfichier  fichier =
	match fichier with
	| [] -> []
	| decl::fichier_restant-> 
		let t = correction'cdecl decl in
		t::(correction'cfichier fichier_restant)
;;

let correction'cletfun fichier = let l = correction'cfichier fichier in (List.rev(!list_letfun))@l;;

(*----------------------------------------------------------------------------*)

let affiche'cident cident =
	match cident with
	|CIdent(s) -> Printf.printf " %s " s
;;

let affiche'cconst cconst =
	match cconst with
	| CEntier(i) -> Printf.printf " %d " i				
	| CAsciiz (i,s) ->  Printf.printf "asciiz[%d]" i
;;

let affiche'cunop cunop =
	match cunop with 
	| CUminus ->  Printf.printf "- "
	| CNot -> Printf.printf "not "
;;

let affiche'cbinop cbinop =
	match cbinop with
	| CAdd ->  Printf.printf "+"
	| CSub ->  Printf.printf "-"
	| CMul ->  Printf.printf "*"
	| CDiv ->  Printf.printf "/"
	| CLe ->  Printf.printf "<="
	| CGe ->  Printf.printf ">="
	| CLt ->  Printf.printf "<"
	| CGt  ->  Printf.printf ">"
	| CNeq ->  Printf.printf "<>"
	| CEq ->  Printf.printf "="
	| CAnd ->  Printf.printf "&&"
	| COr ->  Printf.printf "||"
;;

let rec affiche'cmotif motif = 
	match motif with
	| CJocker ->  Printf.printf "_"
	| CVar(i) ->  affiche'cident i;
	| CAssoc(m1,m2,lm) -> 
		let rec affiche'list_cmotif l = 
			match l with
			| [] -> Printf.printf ") ";
			| x::y ->   Printf.printf ", "; affiche'cmotif x; affiche'list_cmotif y;
			in
			Printf.printf " ("; affiche'cmotif m1 ;  Printf.printf ", "; affiche'cmotif m2; affiche'list_cmotif lm;  
;;


let rec affiche'cexpr expr =
	let rec affiche'list_cexpr lst =
		match lst with
		| [] -> ()
		| x::y -> Printf.printf ", "; affiche'cexpr x; affiche'list_cexpr y;
		in 
	let rec affiche'simple_cexpr simple_expr =
		match simple_expr with
		| CPrint_int ->  Printf.printf "print_int "
		| CPrint_string ->  Printf.printf "print_string "
		| CPrint_newline ->  Printf.printf "print_newline "
		| CRead_int ->  Printf.printf "read_int "
		| CRead_line ->  Printf.printf "read_line "
		| CBracket(e) ->
			Printf.printf "("; affiche'cexpr e; Printf.printf ")";
		| CCalled(i) ->
			affiche'cident i
		| CConst(c) ->
			affiche'cconst c
		| CArg(expr1,expr2,list_expr) ->
			Printf.printf "("; affiche'cexpr expr1; Printf.printf ", "; affiche'cexpr expr2; affiche'list_cexpr list_expr; Printf.printf ")"; 
		in
		match expr with
		| CExpression (simple_expr) ->
			affiche'simple_cexpr simple_expr	
		| CApplication(simple_expr1,simple_expr2) ->
			affiche'simple_cexpr simple_expr1; affiche'simple_cexpr simple_expr2
		| CClos(i,lci) ->
			Printf.printf " Clos fun%d [" i; List.iter (affiche'cident) (lci); Printf.printf "] ";
		| CUnop(cunop,cexpr) ->
			affiche'cunop cunop; affiche'cexpr cexpr;
		| CBinop(cexpr1,cbinop,cexpr2) ->
			affiche'cexpr cexpr1; affiche'cbinop cbinop; affiche'cexpr cexpr2;
		| CEmpile(cexpr1,cexpr2) ->
			affiche'cexpr cexpr1; Printf.printf "::"; affiche'cexpr cexpr2;
		| CIfthenelse(cexpr1,cexpr2,cexpr3) ->
			Printf.printf " if ";affiche'cexpr cexpr1; Printf.printf "\nthen ";affiche'cexpr cexpr2;Printf.printf "\nelse ";affiche'cexpr cexpr3;
		| CLetandin(l,cexpr2) ->
			let rec affiche'let (m,e) = Printf.printf "and "; affiche'cmotif m; Printf.printf " = "; affiche'cexpr e in
			Printf.printf " let "; List.iter affiche'let l;
			Printf.printf "\nin ";affiche'cexpr cexpr2;
		| CLetrecin(m,e,cexpr2) ->
			let rec affiche'let (m,e) = Printf.printf "and "; affiche'cmotif m; Printf.printf " = "; affiche'cexpr e in
			Printf.printf " let rec "; affiche'let (m,e);
			Printf.printf "\nin ";affiche'cexpr cexpr2;
;;

let rec affiche'cdecl cdecl =
	match cdecl with
	| CLetand(l) ->
		let rec affiche'let (m,e) = Printf.printf "and "; affiche'cmotif m; Printf.printf " = "; affiche'cexpr e in
		Printf.printf "let "; List.iter affiche'let l;	
	| CLetrec(m,e) ->
		let rec affiche'let (m,e) = Printf.printf "rec "; affiche'cmotif m; Printf.printf " = "; affiche'cexpr e in
		Printf.printf "let "; affiche'let (m,e);													
	| CLetfun(i,l,m,e) -> 
		Printf.printf "letfun fun%d [" i; List.iter affiche'cident l; Printf.printf "]"; affiche'cmotif m;Printf.printf "=";affiche'cexpr e; 	
;;

let rec affiche'cfichier fichier =
	match fichier with
	| [] -> ()
	| x::y -> affiche'cdecl x; Printf.printf "\n"; affiche'cfichier y;
;;

(*----------------------------------------------------------------------------*)

let vars'cident = function CIdent(s) -> Sset.singleton s;;

let rec vars'cmotif cmotif =
	match cmotif with
	| CJocker -> Sset.empty
	| CVar(cident) -> vars'cident cident
	| CAssoc(cm1,cm2,clm) -> vars'list vars'cmotif ([cm1;cm2]@clm)
;;

let rec vars'cletand f l = 
	match l with
	| [] -> (Sset.empty,Sset.empty)
	| (cm,ce)::y -> (Sset.union (vars'cmotif cm) (fst(vars'cletand f y)), Sset.union (f ce) (snd(vars'cletand f y)))
;;

let rec vars'simple_cexpr f simple_cexpr =
		match simple_cexpr with
		| CBracket(cexpr) -> f cexpr
		| CCalled(cident) -> vars'cident cident
		| CArg(cexpr1,cexpr2,list_cexpr) -> vars'list f ([cexpr1;cexpr2]@list_cexpr)
		| _ -> Sset.empty
;;

let rec vars'cexpr ud cexpr =
	match cexpr with
	| CExpression (simple_cexpr1) ->
		vars'simple_cexpr (vars'cexpr ud) simple_cexpr1		
	| CApplication(simple_cexpr1,simple_cexpr2) ->
		Sset.union (vars'simple_cexpr (vars'cexpr ud) simple_cexpr1) (vars'simple_cexpr (vars'cexpr ud) simple_cexpr2)
	| CClos(i,lci) ->
		vars'list vars'cident lci
	| CUnop(cunop,cexpr) ->
		vars'cexpr ud cexpr	
	| CBinop(cexpr1,cbinop,cexpr2) ->
		Sset.union (vars'cexpr ud cexpr1) (vars'cexpr ud cexpr2)
	| CEmpile(cexpr1,cexpr2) ->
		Sset.union (vars'cexpr ud cexpr1) (vars'cexpr ud cexpr2)
	| CIfthenelse(cexpr1,cexpr2,cexpr3) ->
		vars'list (vars'cexpr ud) [cexpr1;cexpr2;cexpr3]
	| CLetandin(lcme,cexpr) ->
		let (scm,sce) =  vars'cletand (vars'cexpr ud) lcme in 
		Sset.union sce ( ud (vars'cexpr ud cexpr) scm)
	| CLetrecin(cm,ce,cexpr) ->
		let (scm,sce) =  (vars'cmotif cm, vars'cexpr ud ce) in 
		 ud ( Sset.union (vars'cexpr ud cexpr) sce) scm
;;

let vars'cdecl ud cdecl =
	match cdecl with
	| CLetand(lcme) ->
		fst(vars'cletand (vars'cexpr ud) lcme)
	| CLetrec(cm,ce) ->
		ud (vars'cexpr ud ce) (vars'cmotif cm) 
	| CLetfun(i,lci,cm,cexpr) ->
		ud (vars'cexpr ud cexpr) (Sset.union (vars'list vars'cident lci) (vars'cmotif cm))
;;

let rec vars'cfichier ud cfichier =
	match cfichier with
	| [] -> [[]]
	| cdecl::cfichier_restant-> 
		(Sset.elements (vars'cdecl ud cdecl))::(vars'cfichier ud cfichier_restant)
;;

let lvars'cexpr cexpr = Sset.diff (vars'cexpr Sset.union cexpr) (vars'cexpr Sset.diff cexpr);;

