open Ast

val simple_expr_of_exprs : Ast.expr list -> Lexing.position * Lexing.position -> Ast.simple_expr
val function_of_motifs :Ast.motif -> Ast.motif list -> Ast.expr -> Lexing.position * Lexing.position -> Ast.expr
val expr_of_liste_simple_expr : Ast.simple_expr -> Ast.simple_expr list -> Lexing.position * Lexing.position -> Ast.expr
val letand_of_list : (Ast.motif * Ast.expr) list -> Ast.motif list * Ast.expr list 
