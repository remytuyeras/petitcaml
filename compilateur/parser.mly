%{

open Parse
open Ast

%}

%token <int> INTEGER
%token <string> IDENT
%token <string> STRING 
%token COMMA IF THEN ELSE FUNCTION FUN LET
%token ARROW WITH MATCH REC IN PIPE ANDLET
%token LPAREN RPAREN LCROCH RCROCH SEMICOLON DOUBLECOLON DOUBLESEMICOLON JOCKER
%token PLUS MINUS TIMES DIV AND OR NOT TRUE FALSE
%token EQ NEQ LT LE GT GE
%token EOF

%nonassoc IN
%nonassoc ELSE
%nonassoc ARROW
%left OR
%left AND
%left EQ NEQ LT LE GT GE
%right DOUBLECOLON
%left PLUS MINUS
%left TIMES DIV
%nonassoc uminus
%nonassoc NOT

/* Point d'entrée de la grammaire */
%start fichier

/* Type des valeurs retournées par l'analyseur syntaxique */
%type <Ast.fichier> fichier

%%

fichier:
| l = list(terminated(decl,option(DOUBLESEMICOLON))) EOF		{ l }
;

decl:
| LET; m = motif; EQ; e = expr	{ Let(m,e,($startpos,$endpos)) }
//| LET; m1 = motif; EQ; e1 = expr; ANDLET; m2 = motif; EQ; e2 = expr; l = list(preceded(ANDLET,separated_pair(motif,EQ,expr))) { let (lm,le) = Parse.letand_of_list l in Let(Assoc(m1,m2,lm,($startpos,$endpos)),Expression(Arg(e1,e2,le,($startpos,$endpos))),($startpos,$endpos)) }
| LET; i = ident; m = motif; lm = list(motif); EQ; e = expr { Let(Var i,Parse.function_of_motifs m lm e ($startpos,$endpos),($startpos,$endpos)) }
| LET; REC; i = ident; m = motif; lm = list(motif); EQ; e = expr { Letrec(Var i,Parse.function_of_motifs (Var i) (m::lm) e ($startpos,$endpos),($startpos,$endpos)) }
;

motif:
| JOCKER	{ Jocker(($startpos($1),$endpos($1))) }							
| id = ident	{ Var(id)}
| LPAREN; m1 = motif; COMMA; m2 = motif; lm = loption(preceded(COMMA,separated_list(COMMA, motif)));  RPAREN 	{ Assoc(m1,m2,lm,($startpos,$endpos)) }
;

simple_expr:
| LPAREN; e = expr RPAREN 	{ Bracket(e,($startpos,$endpos)) }
| id = ident	{ Called(id) }
| c = const	{ Const(c) }
| LPAREN; e1 = expr; COMMA; e2 = expr; le = loption(preceded(COMMA,separated_list(COMMA,expr))); RPAREN { Arg(e1,e2,le,($startpos,$endpos)) }
| LCROCH; le = separated_list(SEMICOLON, expr); RCROCH { Parse.simple_expr_of_exprs le ($startpos,$endpos) }
;

expr:
| se = simple_expr; lse = list(simple_expr) { Parse.expr_of_liste_simple_expr se lse ($startpos,$endpos) }
| FUNCTION; m = motif; ARROW; e = expr { Function(m,e,($startpos,$endpos)) }
| FUN; m = motif; ARROW; e = expr { Function(m,e,($startpos,$endpos)) }
| NOT; e = expr { Unop(Not(($startpos($1),$endpos($1))),e,($startpos,$endpos)) }
| MINUS; e = expr %prec uminus { Unop(Uminus(($startpos($1),$endpos($1))),e,($startpos,$endpos)) }
| e1 = expr; b = op; e2 = expr { Binop(e1,b,e2,($startpos,$endpos)) }
| e1 = expr; DOUBLECOLON; e2 = expr	{ Empile(e1,e2,($startpos,$endpos)) }
| IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr {  Ifthenelse(e1,e2,e3,($startpos,$endpos)) }
| LET; m = motif; EQ; e1 = expr; IN; e2 = expr	{ Letin(m,e1,e2,($startpos,$endpos)) }
| LET; m1 = motif; EQ; e1 = expr; ANDLET; m2 = motif; EQ; e2 = expr; l = list(preceded(ANDLET,separated_pair(motif,EQ,expr))); IN; e3 = expr { let (lm,le) = Parse.letand_of_list l in Letin(Assoc(m1,m2,lm,($startpos,$endpos)),Expression(Arg(e1,e2,le,($startpos,$endpos))),e3,($startpos,$endpos)) }
| LET; i = ident; m = motif; lm = list(motif); EQ; e1 = expr; IN; e2 = expr { Letin(Var i,Parse.function_of_motifs m lm e1 ($startpos,$endpos), e2,($startpos,$endpos)) }
| LET; REC; i = ident; m = motif; lm = list(motif); EQ; e1 = expr; IN; e2 = expr { Letrecin(Var i,Parse.function_of_motifs (Var i) (m::lm) e1 ($startpos,$endpos), e2, ($startpos,$endpos)) }
| MATCH; e1 = expr; WITH; PIPE?; LCROCH; RCROCH; ARROW; e2 = expr; PIPE; m1 = motif; DOUBLECOLON; m2 = motif; ARROW; e3 = expr { Matchwith(e1,e2,m1,m2,e3,($startpos,$endpos)) }
;

%inline op:
| PLUS  	{ Add(($startpos,$endpos)) } 
| MINUS 	{ Sub(($startpos,$endpos)) }
| TIMES 	{ Mul(($startpos,$endpos)) }
| DIV   	{ Div(($startpos,$endpos)) }
| EQ  		{ Eq(($startpos,$endpos)) }
| NEQ 		{ Neq(($startpos,$endpos)) }
| LT  		{ Lt(($startpos,$endpos)) }
| LE  		{ Le(($startpos,$endpos)) }
| GT  		{ Gt(($startpos,$endpos)) }
| GE  		{ Ge(($startpos,$endpos)) }
| AND  		{ And(($startpos,$endpos)) }
| OR  		{ Or(($startpos,$endpos)) }	
;

const:
| TRUE { True(($startpos,$endpos)) }
| FALSE { False(($startpos,$endpos)) }
| i = INTEGER { Entier(i,($startpos,$endpos)) }
| s = STRING { Chaine(s,($startpos,$endpos)) }
| LPAREN; RPAREN { Nothing(($startpos,$endpos)) }
;

ident:
| i = IDENT { Ident(i,($startpos,$endpos)) }
;
