open Format
open Mips
open Typer
open Spellchecker

let create_lab s = let r = ref 0 in fun () -> incr r; s^(string_of_int !r);;

let labelif = create_lab "Lif";;
let labeland = create_lab "Land";;
let labelor = create_lab "Lor";;
let labelword = create_lab "Lword";;

let active = ref false;;
let frame_size = ref 0;;

let clotures = ref [];;
let data_words = ref [];;
let data_asciiz = ref [];;

let retour = ref [];;
let demande = ref None;;

let push x = Operation(Mips.Sub, SP, SP, Immediate x);;
let pop x = Operation(Mips.Add, SP, SP, Immediate x);;

(* environnements par niveau *)
module Emap = Map.Make(String);;
(*variables avec un label *)
let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17

exception VarUndef of string;;
exception Compilation_echec

let compile'cident env cident =
	match cident with
	| CIdent s ->
			try
				let fp = Emap.find s env in
				(*Printf.printf "compile'cident -> %s (fp = %d)\n%!" s fp;*)
				[Lw (A0, Offseted(-fp, FP))]
			with Not_found ->
				if not (Hashtbl.mem genv s) then raise (VarUndef s);
				(*Printf.printf "compile'cident -> %s (genv)\n%!" s;*)
				[Lw (A0, Labeled("_"^s))]
;;

let rec rassemble'asciiz assoc =
	match assoc with
	| [] -> ()
	| (s,i)::y -> data_asciiz:=(Asciiz("Lasciiz"^(string_of_int i),s)::(!data_asciiz)); rassemble'asciiz y
;;

let compile'cconst cconst =
	match cconst with
	| CEntier(i) -> 
		[Li(A0,i)]				
	| CAsciiz (i,s) -> 
		[La(A0,"Lasciiz"^(string_of_int i))]
;;

(* traduction binop*)
let invertion_of_cunop cunop = 
	match cunop with
	| CUminus -> Mips.Neg
	| CNot -> Mips.Not	
;;

(* traduction unop*)
let operation_of_cbinop cbinop =
	match cbinop with
	| CAdd -> Mips.Add
	| CSub -> Mips.Sub
	| CMul -> Mips.Mul
	| CDiv -> Mips.Div
	| CLe -> Mips.Le
	| CGe -> Mips.Ge
	| CLt -> Mips.Lt
	| CGt -> Mips.Gt
	| CNeq -> Mips.Neq
	| CEq -> Mips.Eq
	| CAnd -> Mips.And
	| COr -> Mips.Or
;;

(* 
.word v : int, bool, unit,
.word p : n-uplet, list, valeur fonctionnelle, string
*)
let rec affecte'cmotif env cmotif =
	match cmotif with
	| CJocker ->
		[]
	| CVar(CIdent(s)) ->
		begin
			try
		    let fp = Emap.find s env in
		    (*Printf.printf "compile'cmotif -> %s (fp = %d)\n%!" s fp;*)
		    [Sw (A0, Offseted(-fp,FP))]
		  with Not_found ->
		    if not (Hashtbl.mem genv s) then raise (VarUndef s);
		    (*Printf.printf "compile'cmotif -> %s (genv)\n%!" s;*)
		    [Sw (A0, Labeled("_"^s))]
		end
	| CAssoc(cm1,cm2,lcm) ->
		let rec partage'cassoc i lst = 
			match lst with
			| [] -> []
			| x::y ->
				[push 4;Sw(A0,Offseted(0,SP))]
				@ [Lw(A0,Offseted(i,A0))]
				@ affecte'cmotif env x
				@ [Lw(A0,Offseted(0,SP));pop 4]
				@ partage'cassoc (i+4) y
		in
		partage'cassoc 0 (cm1::cm2::lcm)
;;

let rec add_local'cmotif cmotif fpm env =
	match cmotif with
	| CJocker -> (env,fpm)
	| CVar(CIdent(s)) ->
		if !active=false
		then
		begin
		if !frame_size = fpm then frame_size := 4 + !frame_size;
		(Emap.add s fpm env,fpm+4);
		end
		else
		(env,fpm)
	| CAssoc(cm1,cm2,lcm) ->
			let rec add_local'list_cmotif lcm fpm env =
				match lcm with
				| [] -> (env,fpm)
				| x::y ->
					let (env,fpm) = add_local'cmotif x fpm env in
					add_local'list_cmotif y fpm env
			in
		let (env,fpm) = add_local'cmotif cm1 fpm env in
		let (env,fpm) = add_local'cmotif cm2 fpm env in
		add_local'list_cmotif lcm fpm env
;;

let rec add_global'cmotif cmotif =
	match cmotif with
	| CJocker -> ()
	| CVar(CIdent(s)) ->
		Hashtbl.replace genv s ()
	| CAssoc(cm1,cm2,lcm) ->
			let rec add_global'list_cmotif lcm =
				match lcm with
				| [] -> ()
				| x::y ->
					add_global'cmotif x;
					add_global'list_cmotif y
			in
		add_global'cmotif cm1;
		add_global'cmotif cm2;
		add_global'list_cmotif lcm
;;

(* evaluation paresseuse des opérateurs logique *)
let compile'logic binop_c c1 c2 l p =
	c1 
	@ [Beq(Register(A0),Immediate p,Labeled(l));
	push 4; Sw(A0,Offseted(0,SP))] 
	@ c2 
	@ [Lw(A1,Offseted(0,SP)); pop 4; 
	Operation(binop_c,A0,A1,Register(A0));
	Label l]
;;

let compile'arith binop_c c1 c2 =
	c1 
	@ [push 4; Sw(A0,Offseted(0,SP))] 
	@ c2 
	@ [Lw(A1,Offseted(0,SP)); pop 4; 
	Operation(binop_c,A0,A1,Register(A0))]
;;

let compile'cbinop cbinop_c c1 c2 =
	match cbinop_c with
	| And -> compile'logic cbinop_c c1 c2 (labeland()) 0
	| Or -> compile'logic cbinop_c c1 c2 (labelor()) 1
	| _ -> compile'arith cbinop_c c1 c2
;;

let rec iter n code = 
	if n=0 
	then [] 
	else  code @ iter (n-1) code
;;

let rec compile'list f env lst =
	match lst with
	| [] -> []
	| x::y -> (f env x)::(compile'list f env y)
;;

let rec initialise'word n = 
	if n <= 0 
	then [] 
	else "1"::(initialise'word (n-1))
;;

let rec affecte'word lcode narg lab = 
		match lcode with
		| [] -> [La(A0,lab)]
		| code::y -> 
			code
			@ [La(T1,lab);
			Sw(A0, Offseted(narg,T1))]
			@ affecte'word y (narg+4) lab 
;;

let compile'wordby f env lst lab rg =
	(*Printf.printf "wordby\n%!"; *)
	let lcode = compile'list f env lst in
		affecte'word lcode rg lab
;;

let record'wordby  = 
	fun f env nbr lst rg  -> 
		let labw =labelword() in
		data_words:=(Word(labw,initialise'word nbr)::!data_words);
		compile'wordby f env lst labw rg
;;

exception Not_found;;

let rec rang i lci ci =
	match lci with
	| [] -> raise Not_found
	| x::y -> if x = ci then i else rang (i+4) y ci
;;

let repondeur lci oci =
	match oci with
	| None -> ()
	| Some ci -> try let rg = rang 4 lci ci in retour:=[Sw(A0, Offseted(rg,A0))] with Not_found -> retour:=[]
;;

let rec compile'cexpr env fpm cexpr =
	let rec compile'simple_cexpr env fpm simple_cexpr =
		match simple_cexpr with
		| CPrint_int -> 		[La(A0,"_0closprint_int")]
		| CPrint_string -> 		[La(A0,"_0closprint_string")]
		| CPrint_newline -> 	[La(A0,"_0closprint_newline")]
		| CRead_int -> 			[La(A0,"_0closread_int")]
		| CRead_line -> 		[La(A0,"_0closread_line")]
		| CBracket(cexpr) -> 	compile'cexpr env fpm cexpr
		| CCalled(cident) -> 	compile'cident env cident
		| CConst(cconst) -> 	compile'cconst cconst
		| CArg(cexpr1,cexpr2,list_cexpr) ->
			let lst = cexpr1::cexpr2::list_cexpr in
			let n = List.length lst in
			record'wordby (fun env -> compile'cexpr env fpm) env n lst 0
	in
		match cexpr with
		| CExpression (simple_cexpr) ->
			compile'simple_cexpr env fpm simple_cexpr
		| CApplication(CPrint_int,simple_cexpr) ->
			let c = compile'simple_cexpr env fpm simple_cexpr in
			c @ [push 4; Sw(RA,Offseted(0,SP));
			Jal("print_int");
			Lw(RA, Offseted(0,SP)); pop 4]
		| CApplication(CPrint_string,simple_cexpr) ->
		 	let c = compile'simple_cexpr env fpm simple_cexpr in
			c @ [push 4; Sw(RA,Offseted(0,SP));
			Jal("print_string");
			Lw(RA, Offseted(0,SP)); pop 4]
		| CApplication(CPrint_newline,simple_cexpr) ->
		 	let c = compile'simple_cexpr env fpm simple_cexpr in
			c @ [push 4; Sw(RA,Offseted(0,SP));
			Jal("print_newline");
			Lw(RA, Offseted(0,SP)); pop 4]
		| CApplication(CRead_int,simple_cexpr) ->
		 	let c = compile'simple_cexpr env fpm simple_cexpr in
			c @ [push 4; Sw(RA,Offseted(0,SP));
			Jal("read_int");
			Lw(RA, Offseted(0,SP)); pop 4]
		| CApplication(CRead_line,simple_cexpr) ->
			let c = compile'simple_cexpr env fpm simple_cexpr in
			c @ [push 4; Sw(RA,Offseted(0,SP));
			Jal("read_line");
			Lw(RA, Offseted(0,SP)); pop 4]		
		| CApplication(simple_cexpr1,simple_cexpr2) ->
			(* on compile e1, dont la valeur doit être un pointeur p1
			vers une fermeture (clos)*)
			let c1 = compile'simple_cexpr env fpm simple_cexpr1
			(* on compile e2, soit v2 sa valeur *)
			and c2 = compile'simple_cexpr env fpm simple_cexpr2 in
			(* on passe les deux arguments p1 et v2 (sur la pile)
			à la fonction ... *)
			[push 4; Sw(RA,Offseted(0,SP))]
			@ c1 @ [push 4;Sw(A0, Offseted(0,SP))]
			@ c2 @ [push 4;Sw(A0, Offseted(0,SP))]
			(* ... dont l'adresse est contenue dans le premier champ de p1 *)
			@ [Lw(T1, Offseted(4,SP)); (* t1 <- p1 *)
			Lw(T1, Offseted(0,T1)); (* t1 <- add(f) *)
			Jalr(T1); (* Jal f *)
			Lw(RA, Offseted(0,SP)); pop 12]
		| CClos(i,lci) ->
			let n = List.length lci in
			let num = string_of_int i in
			(*on alloue un bloc de taille n+1 sur le tas;
			on stocke l'adresse de f dans le champ 0 : 
			f est une étiquette dans le code et on obtient 
			son adresse avec l'instruction MIPS la *)
			let nom_cloture = "_"^num^"clos" and nom_fun = "_"^num^"fun" in
			let fermeture = Word(nom_cloture, nom_fun::(initialise'word n)) in
			clotures:=fermeture::!clotures;
			(* on stocke les valeurs des variables y1,...yn dans les champs 1 à n *)
			repondeur lci !demande;
			compile'wordby compile'cident env lci nom_cloture 4
		| CUnop(cunop,cexpr) ->	
			let cunop_c = invertion_of_cunop cunop 
			and c = compile'cexpr env fpm cexpr in
			c @ [Invertion(cunop_c,A0,A0)]
		| CBinop(cexpr1,cbinop,cexpr2) ->						
			let c1 = compile'cexpr env fpm cexpr1
			and c2 = compile'cexpr env fpm cexpr2
			and cbinop_c = operation_of_cbinop cbinop in
			compile'cbinop cbinop_c c1 c2
		| CEmpile(cexpr1,cexpr2) ->
			record'wordby (fun env -> compile'cexpr env fpm) env 2 [cexpr1;cexpr2] 0
		| CIfthenelse(cexpr1,cexpr2,cexpr3) ->				
			let c1 = compile'cexpr env fpm cexpr1
			and c2 = compile'cexpr env fpm cexpr2
			and c3 = compile'cexpr env fpm cexpr3 in 
			let (lelse,lfi) = (labelif(),labelif()) in 
			c1 @ [Beq(Register(A0),Immediate 0,Labeled(lelse))]
			@ c2 @ [J lfi]
			@ [Label lelse]
			@ c3 @ [Label lfi]
		| CLetandin([],cexpr) ->
			compile'cexpr env fpm cexpr
		| CLetandin((cmotif,cexpr1)::lcme,cexpr2) ->
		    let c1 = compile'cexpr env fpm cexpr1 in
		    let (env,fpm) = add_local'cmotif cmotif fpm env in
		    c1 
		    @ affecte'cmotif env cmotif 
		    @ compile'cexpr env fpm (CLetandin(lcme,cexpr2))
		| CLetrecin(CVar(cident),cexpr1,cexpr2) ->
			let (env,fpm) = add_local'cmotif (CVar(cident)) fpm env in
			demande:= Some cident;
		    let c1 = compile'cexpr env fpm cexpr1 in
		    demande:= None;
		    let cm = affecte'cmotif env (CVar(cident)) @ !retour in
		    c1 
		    @ cm
		    @ compile'cexpr env fpm cexpr2 
		| _ -> raise Compilation_echec 
;;

let rec lcm_of_lci lci =
	match lci with
	| [] -> []
	| x::y -> (CVar(x))::(lcm_of_lci y)
;;

let cm_of_lcm lcm =
	match lcm with
	| [] -> CJocker
	| x::y -> CAssoc(CJocker,x,y)
;;

let rec remplir'env lst fp env =
	match lst with
	| [] -> env
	| x::y -> remplir'env y (fp+4) (Emap.add x fp env)

let code_decl = ref [];;
let code_proc = ref [];;

let rec compile'cdecl env cdecl =
	match cdecl with
	| CLetand([]) ->
		active:=false; 
		()
	| CLetand((cmotif,cexpr)::lcme) ->
		active:=false;
	    let c = compile'cexpr env 0 cexpr in
	    add_global'cmotif cmotif;
	    let code = c @ affecte'cmotif env cmotif in
	    compile'cdecl env (CLetand(lcme));
	    code_decl:=((!code_decl)@ code)
	| CLetrec(CVar(cident),cexpr) ->
		active:=false;
		add_global'cmotif (CVar(cident));
		demande:= Some cident;
	    let c = compile'cexpr env 0 cexpr in
	    code_decl:=((!code_decl) @ c @ affecte'cmotif env (CVar(cident)) @ !retour);
	    demande:= None;
	| CLetfun(i,lcident,CVar(CIdent(s2)),cexpr) ->
		active:=true;
		let s1 = "_"^(string_of_int i)^"ferm"  in
		let lcme = [(cm_of_lcm (lcm_of_lci lcident), CExpression(CCalled(CIdent(s1))))] in
		let cexpr = CLetandin(lcme,cexpr) in
		let lst = Sset.elements (lvars'cexpr cexpr) in
		let n = List.length lst in
		let fs = 4*(n+2) in
		let env = Emap.add s1 (-12) env in
		let env = Emap.add s2 (-8) env in
		let env = remplir'env lst 4 env in
		code_proc:=((!code_proc)@
		[Label ("_"^(string_of_int i)^"fun");
		Operation(Sub,SP,SP,Immediate fs);
		Sw(FP, Offseted(fs-4,SP));
		Sw(RA, Offseted(fs-8,SP));
		Operation(Add,FP,SP,Immediate(fs-8))]
		@ compile'cexpr env 0 cexpr
		@ [Lw(RA, Offseted(0,FP));
		Lw(FP, Offseted(4,FP));
		Operation(Add,SP,SP,Immediate fs);
		Jr RA]
		)
	| CLetfun(i,lcident,cmotif,cexpr) ->
		let ci = CIdent("_"^(string_of_int i)^"arg") in
		let cm = CVar(ci) in
		let cme = (cmotif,CExpression(CCalled(ci))) in
		let lcme = [cme] in
		let ce = CLetandin(lcme,cexpr) in
		compile'cdecl env (CLetfun(i,lcident,cm,ce))
	| _ -> raise Compilation_echec
;;

let rec compile'cfichier fichier =
	match fichier with
	| [] -> ()
	| x::y -> 
		compile'cdecl Emap.empty x;
		compile'cfichier y
;;

(* Compile le fichier et enregistre le code dans le fichier ofile *)
let compile_program cfichier ofile =
	let () = rassemble'asciiz !Spellchecker.assoc'asciiz in
	let () = compile'cfichier cfichier in
	(* On ajoute la sauvegarde de $ra et le code de print *)
	let p = 
    { text = 
        [Label "main";
         Move(S0, RA); (* sauvegarde de $ra *)
         Operation(Sub, SP, SP, Immediate !frame_size); (* alloue la frame *)
         Operation(Add, FP, SP, Immediate (!frame_size - 4)); (* $fp = ... *)
        ] @ 
        (!code_decl )
        @ [Operation(Add, SP, SP, Immediate !frame_size); (* désalloue la frame *)
         Move (RA, S0); (* restaure $ra *)
         Jr RA] @
         (!code_proc)
     	 @ [Label "print_int";
		 Li(V0, 1);
		 Syscall;
		 Jr RA;
		 Label "print_string";
		 Li(V0, 4);
		 Syscall;
		 Jr RA;
		 Label "print_newline";
		 Operation(Sub,SP,SP, Immediate 8);
		 Sw(RA, Offseted(4,SP));
		 Sw(A0, Offseted(0,SP));
		 La(A0, "newline");
		 Jal "print_string";
		 Lw(RA, Offseted(4,SP));
		 Lw(A0, Offseted(0,SP));
		 Operation(Add,SP,SP, Immediate 8);
		 Jr RA;
		 Label "read_int";
		 Li(V0, 5);
		 Syscall;
		 Move(A0,V0);
		 Jr RA;
		 Label "read_line";
		 Operation(Sub,SP,SP, Immediate 4);
		 Sw(A1, Offseted(0,SP));
		 La(A0, "buffer");
		 Li(A1, 1000);
		 Li(V0, 8);
		 Syscall;
		 Lw(A1, Offseted(0,SP));
		 Operation(Add,SP,SP, Immediate 4);
		 Jr RA
        ];
      data = 
			[Space("buffer",1000)]
			@ [Asciiz ("newline", "\"\\n\"")]
			@ (!data_asciiz)
			@ (Hashtbl.fold (fun x _ l -> Word ("_"^x,["1"]) :: l) genv [])
			@ (!data_words)
			@ (!clotures)
			@ [Word("_0closprint_int",["print_int"]);
			Word("_0closprint_string",["print_string"]);
			Word("_0closprint_newline",["print_newline"]);
			Word("_0closread_int",["read_int"]);
			Word("_0closread_line",["read_line"])]
    }
	in
	let f = open_out ofile in 
	let fmt = formatter_of_out_channel f in
	Mips.print_program fmt p;
	(* on "flush" le buffer afin de s'assurer que tout y a été écrit
	 avant de le fermer *)
	fprintf fmt "@?"; 
	close_out f
