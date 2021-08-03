type type_inf =
	| Unit of (Lexing.position * Lexing.position)
	| String of (Lexing.position * Lexing.position)
	| Bool of (Lexing.position * Lexing.position)
	| Integer of (Lexing.position * Lexing.position)
	| Variable of info_var * (Lexing.position * Lexing.position)
	| Arrow of type_inf * type_inf * (Lexing.position * Lexing.position)
	| Uplet of type_inf * type_inf * type_inf list * (Lexing.position * Lexing.position)
	| List of type_inf * (Lexing.position * Lexing.position)

and info_var = { id : int ; mutable instance : type_inf option };;

let nilpos = (Lexing.dummy_pos,Lexing.dummy_pos);;

let rec traduit_inf t =
	match t with
	| Variable({id = _ ; instance = Some a},_) -> traduit_inf a
	| x -> x
;;

let rec position_type t =
	match t with
	| Unit(pos) -> 
		pos
	| String(pos) -> 
		pos
	| Bool(pos) -> 
		pos
	| Integer(pos) -> 
		pos
	| Variable(_,pos) -> 
		pos
	| Arrow(_,_,pos) -> 
		pos
	| Uplet(_,_,_,pos) -> 
		pos
	| List(_,pos) -> 
		pos

(* FONCTIONS D'AFFICHAGE DU TYPAGE : DEBUT *)

let affiche s = print_string s; print_newline();;

let rec string_of_type t =
	let rec convert a u t =
	match t with
	| Unit(_) ->
		"unit"
	| String(_) -> 
		"string"
	| Bool(_) -> 
		"bool"
	| Integer(_) -> 
		"int"
	| Arrow(t1,t2,_) -> 
		if a = 0
			then 
			(convert 2 u t1) ^ " -> " ^ (convert 1 u t2) ^ " = <fun>"
			else 
				if a = 1 
				then (convert 2 u t1) ^ " -> " ^ (convert 1 u t2)
				else "(" ^(convert 2 u t1) ^ " -> " ^ (convert 1 u t2) ^ ")"
	| Uplet(t1,t2,[],_) -> 
		if u = 0
		then (convert 2 1 t1) ^ " * " ^ (convert 2 1 t2)
		else "("^(convert 2 1 t1) ^ " * " ^ (convert 2 1 t2)^")"
	| Uplet(t1,t2,t3::l,_) -> 
		if u = 0
		then (convert 2 1 t1) ^ " * " ^ (convert 2 0 (Uplet(t2,t3,l,nilpos)))
		else "("^(convert 2 1 t1) ^ " * " ^ (convert 2 0 (Uplet(t2,t3,l,nilpos)))^")"
	| List(t1,_) ->  
		(convert 2 u t1) ^ " list"
	| Variable({id = i ;  instance = None},_) -> 
		"'"^(String.make 1 (Char.chr(Char.code('a')+i)))
	| Variable({id = _ ;  instance = Some t1},_) -> 
		convert a u t1 in
	(* mettre en argument 0 0 si on veut que <fun> s'affiche sinon mettre 1 0 *)
	(convert 1 0 t, position_type t)
;;

module Polymorphism = struct
	type t = info_var
	let compare v1 v2 = Pervasives.compare v1.id v2.id
	let filtre_assoc l r e = (e, List.assoc e l)::r
	let filtre l r s = List.fold_left (filtre_assoc l) r s
end
;;

module Pset = Set.Make(Polymorphism)
;;

let rec recup_polymorphe t = 
	match t with
	| Unit(_) ->
		Pset.empty
	| String(_) ->
		Pset.empty
	| Bool(_) ->
		Pset.empty
	| Integer(_) ->
		Pset.empty
	| Arrow(t1,t2,_) ->
		Pset.union (recup_polymorphe t1) (recup_polymorphe t2)
	| Uplet(t1,t2,[],_) ->
		Pset.union (recup_polymorphe t1) (recup_polymorphe t2)
	| Uplet(t1,t2,t3::l,_) ->
		Pset.union (recup_polymorphe t1) (recup_polymorphe (Uplet(t2,t3,l,nilpos)))
	| List(t1,_) ->
		recup_polymorphe t1
	| Variable(w,_) as x ->
		if w.instance = None
		then Pset.singleton w
		else recup_polymorphe (traduit_inf x)
;;

let rec create_assoc n l =
	match l with
	| [] -> []
	| a::b ->  (a,{id = n ; instance = None})::create_assoc (n+1) b
;;

let coherent_polymorphism_comp typ1 typ2 = 
	let rec change t assoc_poly = 
		match t with
		| Unit(_) ->
			t
		| String(_) ->
			t
		| Bool(_) ->
			t
		| Integer(_) ->
			t
		| Arrow(t1,t2,pos) ->
			Arrow(change t1 assoc_poly,change t2 assoc_poly,pos)
		| Uplet(t1,t2,l,pos) ->
			let rec change_list l assoc_poly = 
				match l with
				| [] -> []
				| x::y -> (change x assoc_poly)::(change_list y assoc_poly)
				in
			Uplet(change t1 assoc_poly,change t2 assoc_poly,change_list l assoc_poly,pos)
		| List(t1,pos) ->
			List (change t1 assoc_poly,pos)
		| Variable(w,pos) ->
			if w.instance = None
				then
					let (v,u) = assoc_poly in
					if Polymorphism.compare w v = 0
						then Variable(u,pos)
						else Variable(w,pos)
				else change (traduit_inf t) assoc_poly
	in
	let r1 = recup_polymorphe typ1 
	and r2 = recup_polymorphe typ2 in
	let lassoc = create_assoc 0 (Pset.elements (Pset.union r1 r2)) in
	let l1 = Polymorphism.filtre lassoc [] (Pset.elements r1)
	and l2 = Polymorphism.filtre lassoc [] (Pset.elements r2) in
	let x1 = List.fold_left change typ1 l1
	and x2 = List.fold_left change typ2 l2 in
	(string_of_type x1, string_of_type x2)
;;

let coherent_polymorphism typ1 = 
	let rec change t assoc_poly = 
		match t with
		| Unit(_)->
			t
		| String(_) ->
			t
		| Bool(_)->
			t
		| Integer(_)->
			t
		| Arrow(t1,t2,pos) ->
			Arrow(change t1 assoc_poly,change t2 assoc_poly,pos)
		| Uplet(t1,t2,l,pos) ->
			let rec change_list l assoc_poly = 
				match l with
				| [] -> []
				| x::y -> (change x assoc_poly)::(change_list y assoc_poly)
				in
			Uplet(change t1 assoc_poly,change t2 assoc_poly,change_list l assoc_poly,pos)
		| List(t1,pos) ->
			List (change t1 assoc_poly,pos)
		| Variable(w,pos) ->
			if w.instance = None
			then
				let (v,u) = assoc_poly in
				if Polymorphism.compare w v = 0
					then Variable(u,pos)
					else Variable(w,pos)
			else change (traduit_inf t) assoc_poly 
			in
	let lassoc =  create_assoc 0 (Pset.elements (recup_polymorphe typ1))  in
	let t1 = List.fold_left change typ1 lassoc
	in string_of_type t1
;;

(* FONCTIONS D'AFFICHAGES DU TYPAGE : FIN *)

exception Unification_echec of ((string * (Lexing.position * Lexing.position)) * (string * (Lexing.position * Lexing.position)));;

let unification_erreur t1 t2 = raise (Unification_echec(coherent_polymorphism_comp t1 t2));;

let rec occur v t = 
	match t with
	| Unit(_) ->
		false
	| String(_) ->
		false
	| Bool(_) ->
		false
	| Integer(_) ->
		false
	| Arrow(t1,t2,_) -> 
		occur v t1 || occur v t2
	| Uplet(t1,t2,[],_) -> 
		occur v t1 || occur v t2
	| Uplet(t1,t2,t3::l,_) ->
		occur v t1 || occur v (Uplet(t2,t3,l,nilpos))
	| List(t1,_) ->
		occur v t1
	| Variable(w,_) as x->
		if w.instance = None
			then
				if compare w.id v.id = 0 
				then true
				else false
			else occur v (traduit_inf x)
		
		(*if compare w.id v.id = 0 
			then true
			else
				if w.instance = None
					then false
					else occur v (traduit_inf x)*)
;;

let rec unifier t1 t2 = 
	(*Printf.printf " unifie -> {%s} | {%s}\n%!" (fst(string_of_type t1)) (fst(string_of_type t2));*)
	match (t1,t2) with
	| (Unit(_),Unit(_)) ->  
		()
	| (String(_),String(_)) -> 
		()
	| (Bool(_),Bool(_)) ->  
		()
	| (Integer(_),Integer(_)) -> 
		()
	| (Variable(a,_),Variable(b,_)) when a = b -> 
		()
	| (Arrow(x1,y1,_),Arrow(x2,y2,_)) -> 
		unifier x1 x2 ; 
		unifier y1 y2 ; 
	| (Uplet(a1,a2,[],_),Uplet(b1,b2,[],_)) -> 
		unifier a1 b1 ; 
		unifier a2 b2 ;
	| (Uplet(a1,a2,a3::la,_),Uplet(b1,b2,b3::lb,_)) -> 
		unifier a1 b1 ; 
		unifier (Uplet(a2,a3,la,nilpos)) (Uplet(b2,b3,lb,nilpos)); 
	| (List(z1,_), List(z2,_)) -> 
		unifier z1 z2 ;		
	| (Variable(w,_),t) as x ->
			if w.instance = None
				then 
					begin
					(* pour éviter les boucles infinis *)
					if occur w t = false
						then  w.instance <- Some (traduit_inf t)
						else unification_erreur t1 t2
					end
				else
					begin
					let (a,b) = x in unifier (traduit_inf a) b
					end
	| (t,Variable(w,_)) ->
		unifier t2 t1; 				
	| _ -> 
		unification_erreur t1 t2;
;;

module V = struct
	type t = info_var
	let compare v1 v2 = Pervasives.compare v1.id v2.id 
	let create = let r = ref 0 in fun () -> incr r; {id= !r; instance = None}
end
;;

module Vset = Set.Make(V);;

let rec recup_fvars t = 
	match t with
	| Unit(_) ->
		Vset.empty
	| String(_) ->
		Vset.empty
	| Bool(_) ->
		Vset.empty
	| Integer(_) ->
		Vset.empty
	| Arrow(t1,t2,_) ->
		Vset.union (recup_fvars t1) (recup_fvars t2)
	| Uplet(t1,t2,[],_) ->
		Vset.union (recup_fvars t1) (recup_fvars t2)
	| Uplet(t1,t2,t3::l,_) ->
		Vset.union (recup_fvars t1) (recup_fvars (Uplet(t2,t3,l,nilpos)))
	| List(t1,_) ->
		recup_fvars t1
	| Variable(w,_) as x ->
		if w.instance = None
		then Vset.singleton w
		else recup_fvars (traduit_inf x)
;;

type schema = { vars : Vset.t; typ : type_inf };;

module Smap = Map.Make(String);;

type env = { bindings : schema Smap.t ; fvars : Vset.t };;

let empty = { bindings = Smap.empty ; fvars = Vset.empty };;

(* re-calcule les variables de type libre restante *)

let new_fvars b =
	let f v set = Vset.union (recup_fvars (Variable(v,nilpos))) set
	in
	Vset.fold f b Vset.empty
;;

(* e (environnement) n'est pas modifier par effet de bord mais par valeur*)
(*seule une partie de la pile de récursion considerera un environnement donné*)

(* add : relatif à un environement de définition intrasec à une fonction *)
(* par exemple pour fun X -> Y, alors on travail dans [fun X ->{ }] et on cherche Env,X |= Y ?   *)
(* on a donc aucune variable libre, les autres variables étant globales (externes)*)

let add key t env =
	match env with
	| { bindings = a ; fvars = b } -> 
		let x = recup_fvars t 
		and y = new_fvars b
		and sch = { vars = Vset.empty ; typ = t }
		in
		{ bindings = Smap.add key sch a ; fvars = Vset.union x y }  (*union utile*)
;;

(* add_gen: ici on a du polymorphisme car le [in] agit comme un [;;] mais pour un mini environement*)
(* ici on travail dans [let X = Y in { }] et on y fait agir X tant qu'on peut *)
(* ON DEVRA FAIRE COMME CA POUR UN ENCHAINEMENT DE DECLARATIONS*)

let add_gen key t env =
	match env with
	| { bindings = a ; fvars = b } -> 
		let x = recup_fvars t and y = new_fvars b
		in
		let sch = { vars = Vset.diff x y ; typ = t }
		in
		{ bindings = Smap.add key sch a ; fvars = y } (*union inutile*)
;;

(* pour [let rec] c'est un peu comme add_gen mais on doit mettre [var] dans bindings*)
(* sinon Smap.find dans find retourne un Not_found *)
(* car si on regarde Let(var,expr1,expr2), var n'est mis que pour expr2 *)
(* car pour déterminer le type de [var] il faut dabord évaluer expr1 *)
(* on donnera le type 'a -> 'b à [var] avec  la fct add : on peut voir let f = fun x -> fun f -> f x *)
(* pour s'en assurer faire dans ocaml : let rec f x = f x ;; et let rec f =f ;; et enfin let rec f =1;;*)


(* Ici on travail sur la fonction find *)
(* Afin que le polymorphisme soit possible il faut réinitialiser le a de &a.a->a après utilisation*)
(* on lui donne donc une nouvelle variable *)

let rec genere_assoc lst =
	match lst with
	| [] -> []
	| a::l -> (a,V.create())::genere_assoc l
;;

let rec vars_of_assoc l =
	match l with
	| [] -> Vset.empty
	| a::b -> let (x,y) =a in Vset.union (Vset.singleton y) (vars_of_assoc b)
;;

let refresh_schema vars typ = 
	let rec change t assoc_var = 
		match t with
		| Unit(_) ->
			t
		| String(_) ->
			t
		| Bool(_) ->
			t
		| Integer(_) ->
			t
		| Arrow(t1,t2,pos) ->
			Arrow(change t1 assoc_var,change t2 assoc_var,pos)
		| Uplet(t1,t2,l,pos) ->
			let rec change_list l assoc_var = 
				match l with
				| [] -> []
				| x::y -> (change x assoc_var)::(change_list y assoc_var)
				in
			Uplet(change t1 assoc_var,change t2 assoc_var,change_list l assoc_var,pos)
		| List(t1,pos) ->
			List (change t1 assoc_var,pos)
		| Variable(w,pos) ->
			if w.instance = None
				then
					let (v,u) = assoc_var in
					if V.compare w v = 0
						then Variable(u,pos)
						else Variable(w,pos)
				else change (traduit_inf t) assoc_var
	in
	let lst = genere_assoc (Vset.elements vars) in
	{ vars = (vars_of_assoc lst) ; typ = (List.fold_left change typ lst) }
;;

let find key env =
	match env with
	| { bindings = a ; fvars = b } -> 
		let sch1 = Smap.find key a in
		let sch2 = refresh_schema sch1.vars sch1.typ in
		ignore (Smap.add key sch2 a);
		sch2.typ
;;


open Ast

(*IDENT*)

exception Unbound_variable of string * (Lexing.position * Lexing.position)

let type_addg = ref [];;
let type_add = ref [];;
let type_rec = ref [];;

let w'ident env ident =
	match ident with
	| Ident(s,pos) ->
		match s with
		| "print_int" -> Arrow(Integer(pos),Unit(pos),pos);
		| "print_string" -> Arrow(String(pos),Unit(pos),pos);
		| "print_newline" -> Arrow(Unit(pos),Unit(pos),pos);
		| "read_int" -> Arrow(Unit(pos),Integer(pos),pos);
		| "read_line" -> Arrow(Unit(pos),String(pos),pos);
		| _ -> try find s env with Not_found -> raise (Unbound_variable(s,pos));
;;

let name_of_ident ident =
	match ident with
	Ident(s,_) -> s
;;

let pos_of_ident ident =
	match ident with
	Ident(_,p) -> p
;;

(*CONST*)

let w'const const =
	match const with
	| True(pos) | False(pos) ->
		Bool(pos)
	| Entier(i,pos) ->
		Integer(pos)
	| Chaine(s,pos) ->
		String(pos)
	| Nothing(pos) ->
		Unit(pos)
;;

(*UNOP*)

let w'unop unop = 
	match unop with
	| Uminus(pos) ->
		Arrow(Integer(pos),Integer(pos),pos)
	| Not(pos) ->
		Arrow(Bool(pos),Bool(pos),pos)
;;

(*BINOP*)

let w'binop binop =
	match binop with
	| Add(pos) | Sub(pos) | Mul(pos) | Div(pos) ->
		Arrow(Uplet(Integer(pos),Integer(pos),[],pos),Integer(pos),pos)
	| Le(pos) | Ge(pos) | Lt(pos)| Gt(pos) | Neq(pos) | Eq(pos) ->
		Arrow(Uplet(Integer(pos),Integer(pos),[],pos),Bool(pos),pos)
	| And(pos) | Or(pos) ->
		Arrow(Uplet(Bool(pos),Bool(pos),[],pos),Bool(pos),pos)
;;

(*MOTIF*)

exception Several_bindings_echec of string * (Lexing.position * Lexing.position);;

let rec unique l = 
	match l with
	| [] -> 
		("",false)
	| x::y ->
		let (key,info) = x in
		if List.mem_assoc key y = true
		then (key,true)
		else unique y
;;

let w'motif_unique env motif type_motif =
	let rec w'motif env m pile type_motif =
		match m with
		| Jocker(pos) ->
			(env,Variable (V.create (),pos))
		| Var(i) ->
			let s = name_of_ident i and pos = pos_of_ident i in
			pile:=(s,pos_of_ident i)::(!pile);
			let alpha = V.create () in 
			type_motif:=(!type_motif@[(s,Variable(alpha,pos))]);
			(add s (Variable(alpha,pos)) env, Variable(alpha,pos))
		| Assoc(m1,m2,listm,pos) ->
			let rec w'list_motif env lm pile type_motif =
				match lm with
				| [] -> 
					(env,[])
				| x::y -> 
					let (env,lx) = w'motif env x pile type_motif in
					let (env,ly) = w'list_motif env y pile type_motif in
					(env,lx::ly)
			in
		let (env,t1) = w'motif env m1 pile type_motif in 
		let (env,t2) = w'motif env m2 pile type_motif in
		let (env,lt) = w'list_motif env listm pile type_motif in
		(env,Uplet(t1,t2,lt,pos))
	in
	let pile = ref [] in 
	let resultat = w'motif env motif pile type_motif in
	let (s,b) = unique !pile in
	if b = true
	then raise (Several_bindings_echec(s,List.assoc s !pile))
	else resultat
;;
				
let rec add_gen_motif motif type_expr env type_motif=
	match (motif,type_expr) with
	| (Jocker(_),_) ->
		env
	| (Var(i),t) ->
		let s = name_of_ident i in
		type_motif:=(!type_motif@[(s,t)]);
		add_gen s t env
	| (Assoc(m1,m2,listm,_),Uplet(t1,t2,listt,_)) ->
		let rec add_gen_list_motif lm lt env type_motif =
			match (lm,lt) with
			| ([],[]) -> 
				env
			| (xm::ym,xt::yt) -> 
				let env1 = add_gen_motif xm xt env type_motif in
				add_gen_list_motif ym yt env1 type_motif
			| _ -> 
				env
		in
		let env = add_gen_motif m1 t1 env type_motif in 
		let env = add_gen_motif m2 t2 env type_motif in
		add_gen_list_motif listm listt env type_motif
	| (Assoc(_,_,_,_),Variable(_,_)) ->
		add_gen_motif motif (traduit_inf type_expr)	env type_motif
	| (_,_) ->
		env
;;

(*EXPR*)

let flag_rec = ref 0;;

let rec w'expr env expr =
	let rec w'list_expr env lst =
		match lst with
		| [] -> []
		| x::y -> (w'expr env x)::(w'list_expr env y)
		in 
	let rec w'simple_expr env simple_expr =
		match simple_expr with
		| Bracket(expr,pos) ->
			w'expr env expr
		| Called(ident) ->
			w'ident env ident
		| Const(const) ->
			w'const const
		| Arg(expr1,expr2,list_expr,pos) ->
			let t1 = w'expr env expr1
			and t2 = w'expr env expr2
			and tl = w'list_expr env list_expr in
			Uplet(t1,t2,tl,pos)
		| Liste(pos) ->
			List(Variable(V.create(),pos),pos)
		in
		match expr with
		| Expression (simple_expr) ->
			w'simple_expr env simple_expr		
		| Application(simple_expr1,simple_expr2,pos) ->
			let t1 = w'simple_expr env simple_expr1 
			and t2 = w'simple_expr env simple_expr2
			and alpha = V.create () in
			unifier t1 (Arrow(t2,Variable(alpha,pos),pos));
			Variable(alpha,pos)
		| Function(motif,expr,pos) ->
			if !flag_rec = 0
			then
			let (envm,tm) = w'motif_unique env motif type_add in
			let t = w'expr envm expr in 
			Arrow(tm,t,pos)
			else 
			begin
			flag_rec:=0;
			let (envm,tm) = w'motif_unique env motif type_rec in
			let t = w'expr envm expr in 
			Arrow(tm,t,pos)
			end
		| Unop(unop,expr,pos) ->
			let unop_type = w'unop unop 
			and t = w'expr env expr 
			and alpha = V.create () in
			unifier unop_type (Arrow(t,Variable(alpha,pos),pos));
			Variable(alpha,pos)
		| Binop(expr1,binop,expr2,pos) ->
			let t1 = w'expr env expr1
			and t2 = w'expr env expr2
			and binop_type = w'binop binop
			and alpha = V.create () in
			unifier binop_type (Arrow(Uplet(t1,t2,[],pos),Variable(alpha,pos),pos));
			Variable(alpha,pos)
		| Empile(expr1,expr2,pos) ->
			let t1 = w'expr env expr1
			and t2 = w'expr env expr2 in
			unifier t2 (List(t1,pos));			
			t2
		| Ifthenelse(expr1,expr2,expr3,pos) ->
			let t1 = w'expr env expr1
			and t2 = w'expr env expr2
			and t3 = w'expr env expr3
			and ite_type = Variable (V.create (),pos) in 
			let opif_type = Arrow(Uplet(Bool(pos),ite_type,[ite_type],pos),ite_type,pos) in
			unifier (Arrow(Uplet(t1,t2,[t3],pos),t2,pos)) opif_type;
			ite_type
		| Letin(motif,expr1,expr2,pos) ->
			let t = w'expr env expr1
			and (envm,tm) = w'motif_unique env motif type_add in
			unifier tm t;
			w'expr (add_gen_motif motif t env type_addg) expr2
		| Letrecin(motif,expr1,expr2,pos) ->
			flag_rec:=1;
			let trec = Arrow(Variable (V.create (),pos),Variable (V.create (),pos),pos)
			and t = w'expr env expr1 in
			unifier t (Arrow(trec,trec,pos));
			w'expr (add_gen_motif motif trec env type_addg) expr2 
		| Matchwith(expr1,expr2,motif1,motif2,expr3,pos) ->
			let (env1,t1) = w'motif_unique env motif1 type_add in
			let (env2,t2) = w'motif_unique env1 motif2 type_add in
			let alpha = V.create () and beta = V.create () in
			unifier (w'expr env expr1)  (List (Variable (alpha,pos),pos));
			unifier t1 (Variable (alpha,pos));
			unifier t2 (List((Variable (alpha,pos)),pos));
			unifier (w'expr env expr2) (Variable(beta,pos));
			unifier (w'expr env2 expr3) (Variable(beta,pos));
			Variable(beta,pos)
;;

(*DECL*)

let w'decl env decl =
	match decl with
	| Let(motif,expr,pos) ->
		let t = w'expr env expr
		and (envm,tm) = w'motif_unique env motif type_add in
		unifier tm t;
		(add_gen_motif motif t env type_addg,t)
	| Letrec(motif,expr,pos) ->
		flag_rec:=1;
		let trec = Arrow(Variable(V.create(),pos),Variable(V.create(),pos),pos)
		and t = w'expr env expr in
		unifier t (Arrow(trec,trec,pos)) ;
		(add_gen_motif motif trec env type_addg, trec)
;;

(*FICHIER*)

let rec w'fichier env fichier =
	match fichier with
	| [] -> 
		[]
	| decl::fichier_restant-> 
		let (env',t) = w'decl env decl in
		t::(w'fichier env' fichier_restant)
;;

let rec typeof lst = 
	match lst with
	| [] -> ()
	| x::l -> let (s,pos) = coherent_polymorphism x in affiche ("type reconnu : "^s) ; typeof l
;;

let algorithme_w p = w'fichier empty p;;
