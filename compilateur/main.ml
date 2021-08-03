(* Fichier principal du compilateur*)

open Format (* fournit les fonctions d'affichage comme eprintf; voir la doc pour les formats avec @*)
open Lexing (* les informations contenues dans pos*)

(* Option de compilation, pour s'arrêter à l'issue du parser *)
let parse_only = ref false;;
(* Option de compilation, pour s'arrêter à l'issue du typer *)
let type_only = ref false;;
(* Option de compilation, pour afficher les types reconnus *)
let typing = ref false;;

(* Noms des fichiers source et cible *)
let ifile = ref "";;
let ofile = ref "";;

let set_file f s = f := s;;

(* Les fonctions du module Arg prenne en arguments les éléments de la ligne de commande*)
(* Arg.set : met à vrai la variable de type : bool ref *)
(* Arg.string: prend une fonction string -> unit et lui fait lire le mot qui suit l'option*)

(* Les options du compilateur que l'on affiche en tapant petitcaml --help *)
let options = 
  ["-parse-only", Arg.Set parse_only, "  Pour stopper la compilation après l'analyse syntaxique";
   "-type-only", Arg.Set type_only, "  Pour stopper la compilation après l'analyse sémantique";
   "-typing", Arg.Set typing, "  Pour afficher le type des déclarations dans l'ordre de leur apparition dans le fichier";
   "-o", Arg.String (set_file ofile), "<file>  Pour indiquer le mom du fichier de sortie"]
;;

let usage = "usage: petitcaml [option] file.ml";;

(* localise une erreur en indiquant la ligne et la colonne *)
(* voir le type position dans Lexing *)
(* pos_lnum = num de ligne ; pos_bol = num beginning of line ; pos_cnum = num de char*)
let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l c (c+1)
;;

let localisation_ast posd posf=
  let ld = posd.pos_lnum and lf = posf.pos_lnum in
  let cd = posd.pos_cnum - posd.pos_bol+1 and cf = posf.pos_cnum - posf.pos_bol+1 in
  if ld = lf
  then eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile ld cd (cf+1)
  else eprintf "File \"%s\", line %d, characters %d to line %d, characters %d :\n" !ifile ld cd lf (cf+1)
;;


(*le programme principale dont, apparement, on se contre-fout complétement du nom *)

let () = 
(* Arg.parse parse les ARGUMENTS de la ligne de commande *)
(* type : (key, spec, doc) list -> (string -> unit) -> string -> unit *)
(* (key, spec, doc) list : parse les key, actionne les spec, et si besoin donne doc*)
(* donne le nom du FICHIER passé en commande à la fonction  (set_file ifile) *)
(* l'ordre d'appel est l'ordre dans la ligne de commande *)
(* si erreur, le programme affiche: *)
(* -> la propre analyse d'erreur de Arg.parse *)
(* -> le troisieme argument, ici : usage *)
(* -> les doc de options dans l'ordre + les doc propre à Arg.parse : -help et --help *)
  Arg.parse options (set_file ifile) usage;

  (* On vérifie que le nom du fichier source a bien été indiqué *)
  (* @? est une sorte de flush formaté*)
  if !ifile="" then begin eprintf "Aucun fichier à compiler\n@?"; exit 1 end; 

  (* Ce fichier doit avoir l'extension .ml *)
  (* check_suffix parse les suffixes et retourne vrai si .ml en est *)
  if not (Filename.check_suffix !ifile ".ml") then begin
    eprintf "Le fichier d'entrée doit avoir l'extension .ml\n@?";
    (* affiche le tralala de options et usage*)
    Arg.usage options usage;
    exit 1
  end;

  (* Par défaut, le fichier cible a le même nom que le fichier source, 
     seule l'extension change *)
  (* chop_suffix supprime le suffixe .ml si il y est, sinon on sait pas *)
  if !ofile="" then ofile := Filename.chop_suffix !ifile ".ml" ^ ".s";
  
  (* Ouverture du fichier source en lecture *)
  (* open_in est dans Pervasives : équivalent de fopen en C avec <<r>> *)
  (* string -> in_channel : retourne un pointeur sur le debut du fichier en mode input*)
  let f = open_in !ifile in
    
  (* Création d'un tampon d'analyse lexicale *)
  (* from_channel in_channel -> lexbuf*)
  (* fournit un lexer sur la position courante du fichier : ici, le début*)
  let buf = Lexing.from_channel f in
  
  try
    (* Parsing: la fonction  Parser.prog transforme le tampon lexical en un 
       arbre de syntaxe abstraite si aucune erreur (lexicale ou syntaxique) 
       n'est détectée.
       La fonction Lexer.token est utilisée par Parser.prog pour obtenir 
       le prochain token. *)
    (* token est le nom de la rule dans lexer*)
    (* prog est le nom de l'arbre retourné par paser de type <Ast.fichier> *)
    (* fichier est la syntaxe abstraite pour le programme *)
    let p = Parser.fichier Lexer.token buf in
    close_in f;
    
    (* On s'arrête ici si on ne veut faire que le parsing *)
    if !parse_only then exit 0;

    (*Ici on applique l'algorithme w qui se trouve dans typer*)
    let liste_type = Typer.algorithme_w p in
    
    if !typing then Typer.typeof(liste_type);
    
    if !type_only then exit 0;
    
    let cp = Spellchecker.correction'cletfun p  in 
    (*Spellchecker.affiche'cfichier cp;*)
    
    (* Compilation de l'arbre de syntaxe abstraite p. Le code machine 
       résultant de cette transformation doit être écrit dans le fichier 
       cible ofile. *)
    Compiler.compile_program cp !ofile;
  
  with
  (* ici le format @. flush puis va à la ligne*)
    | Lexer.Lexing_error s -> 
	(* Erreur lexicale. On récupère sa position absolue et 
	   on la convertit en numéro de ligne *)
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Illegal character: %s@." s;
	exit 1
    | Parser.Error -> 
	(* Erreur syntaxique. On récupère sa position absolue et on la 
	   convertit en numéro de ligne *)
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Syntax error@.";
	exit 1
	| Typer.Unbound_variable(s,(posd,posf)) ->
	(*Erreur sémantique. On récupère sa position absolue et on la 
	   convertit en numéro de ligne *)
	localisation_ast posd posf;
    eprintf "Unbound value %s@." s;
	exit 1
    | Typer.Unification_echec(((s1,(posd1,posf1)),(s2,(posd2,posf2)))) ->
    (*Erreur sémantique. On récupère sa position absolue et on la 
	   convertit en numéro de ligne *)
	if posd2.pos_lnum > posd1.pos_lnum || (posd1,posf1) = Typer.nilpos
	then 
	begin
	localisation_ast posd2 posf2;
	eprintf "This expression has type %s but is here used with type %s@." s1 s2;
	exit 1 
	end
	else 
	begin
	localisation_ast posd1 posf1;
	eprintf "This expression has type %s but is here used with type %s@." s2 s1;
	exit 1
	end
	| Typer.Several_bindings_echec(s,(posd,posf)) ->
	(*Erreur sémantique. On récupère sa position absolue et on la 
	   convertit en numéro de ligne *)
	localisation_ast posd posf;
    eprintf "The variable %s is bound several times in this matching@." s;
	exit 1
	| Spellchecker.Correction_echec ->
	eprintf "Erreur à la correction vers mips@.";
	exit 2
    | Compiler.VarUndef s -> 
	(* Erreur d'utilisation de variable pendant la compilation *)
	eprintf "Erreur de compilation: la variable %s n'est pas définie@." s;
	exit 2




