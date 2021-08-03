
type register = 
| ZERO
| AT
| V0 | V1
| A0 | A1 | A2 | A3
| T0 | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9
| S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7
| K0 | K1
| GP
| SP
| FP
| RA

type address =
| Labeled 		of string
| Offseted 		of int * register

type operand =
| Immediate 	of int
| Register 		of register

type operation = Add | Sub | Mul | Div | And | Or | Eq | Neq | Ge | Gt | Le | Lt 

type invertion = Neg | Not

type instruction =
| Move 			of register * register
| Li 			of register * int
| La 			of register * string
| Lw 			of register * address
| Sw 			of register * address
| Beq	 		of operand * operand * address
| Operation 	of operation * register * register * operand
| Invertion 	of invertion * register * register
| Jal 			of string
| Jalr 			of register
| Jr			of register
| Label 		of string
| J				of string
| Syscall
| Nop

type data = 
| Asciiz 		of string * string
| Word 			of string * string list
| Space			of string * int

type program = {
text : instruction list;
data : data list;
}

open Format

let print_register fmt = function
| ZERO -> pp_print_string fmt "$zero"
| AT -> pp_print_string fmt "$at"
| V0 -> pp_print_string fmt "$v0"
| V1 -> pp_print_string fmt "$v1"
| A0 -> pp_print_string fmt "$a0"
| A1 -> pp_print_string fmt "$a1"
| A2 -> pp_print_string fmt "$a2"
| A3 -> pp_print_string fmt "$a3"
| T0 -> pp_print_string fmt "$t0"
| T1 -> pp_print_string fmt "$t1"
| T2 -> pp_print_string fmt "$t2"
| T3 -> pp_print_string fmt "$t3"
| T4 -> pp_print_string fmt "$t4"
| T5 -> pp_print_string fmt "$t5"
| T6 -> pp_print_string fmt "$t6"
| T7 -> pp_print_string fmt "$t7"
| T8 -> pp_print_string fmt "$t8"
| T9 -> pp_print_string fmt "$t9"
| S0 -> pp_print_string fmt "$s0"
| S1 -> pp_print_string fmt "$s1"
| S2 -> pp_print_string fmt "$s2"
| S3 -> pp_print_string fmt "$s3"
| S4 -> pp_print_string fmt "$s4"
| S5 -> pp_print_string fmt "$s5"
| S6 -> pp_print_string fmt "$s6"
| S7 -> pp_print_string fmt "$s7"
| K0 -> pp_print_string fmt "$k0"
| K1 -> pp_print_string fmt "$k1"
| GP -> pp_print_string fmt "$gp"
| SP -> pp_print_string fmt "$sp"
| FP -> pp_print_string fmt "$fp"
| RA -> pp_print_string fmt "$ra"

let print_operation fmt = function
| Add -> pp_print_string fmt "add"
| Sub -> pp_print_string fmt "sub"
| Mul -> pp_print_string fmt "mul"
| Div -> pp_print_string fmt "div"
| And -> pp_print_string fmt "and"
| Or -> pp_print_string fmt "or"
| Eq -> pp_print_string fmt "seq"
| Neq -> pp_print_string fmt "sne"
| Ge -> pp_print_string fmt "sge"
| Gt -> pp_print_string fmt "sgt"
| Le -> pp_print_string fmt "sle"
| Lt -> pp_print_string fmt "slt"

let print_invertion fmt = function
| Not -> pp_print_string fmt "not"
| Neg -> pp_print_string fmt "neg"

let print_address fmt = function
| Labeled s -> pp_print_string fmt s
| Offseted (ofs, r) -> fprintf fmt "%d(%a)" ofs print_register r

let print_operand fmt = function
| Immediate i -> pp_print_int fmt i
| Register r -> print_register fmt r

(* cf dans le module Format : %a -> prend deux arguments *)
(* applique le premier au deuxième *)
(* les types des deux arguments dans l'ordre : out_channel->'b->unit et 'b *)

let print_instruction fmt = function
| Move (dst, src) -> 
	fprintf fmt "\tmove\t%a, %a\n" print_register dst print_register src
| Li (r, i) ->
	fprintf fmt "\tli\t%a, %d\n" print_register r i
| La (r, s) ->
	fprintf fmt "\tla\t%a, %s\n" print_register r s
| Lw (r, a) ->
	fprintf fmt "\tlw\t%a, %a\n" print_register r print_address a
| Sw (r, a) ->
	fprintf fmt "\tsw\t%a, %a\n" print_register r print_address a
| Beq (rs,rt, a) ->
	fprintf fmt "\tbeq\t%a, %a, %a\n" print_operand rs print_operand rt print_address a
| Operation (o, dst, src, op) ->
	fprintf fmt "\t%a\t%a, %a, %a\n" print_operation o print_register dst print_register src print_operand op
| Invertion (i, dst, src) ->
	fprintf fmt "\t%a\t%a, %a\n" print_invertion i print_register dst print_register src
| Jal s ->
	fprintf fmt "\tjal\t%s\n" s
| Jalr r ->
	fprintf fmt "\tjalr\t%a\n" print_register r
| Jr r ->
	fprintf fmt "\tjr\t%a\n" print_register r
| Label s ->
	fprintf fmt "%s:\n" s
| J s ->
	fprintf fmt "\tj\t%s\n" s
| Nop ->
	fprintf fmt "\tnop\n"
| Syscall ->
	fprintf fmt "\tsyscall\n"

let print_data fmt = function
| Asciiz (l, s) -> 
	fprintf fmt "%s:\t.asciiz %s\n" l s
| Word (l, lst) ->
	fprintf fmt "%s:\t.word" l; List.iter (fprintf fmt " %s") lst; fprintf fmt "\n"
| Space (l, n) ->
	fprintf fmt "%s:\t.space %d\n" l n

let print_program fmt p =
	fprintf fmt "\t.text\n";
	List.iter (print_instruction fmt) p.text;
	fprintf fmt "\t.data\n";
	List.iter (print_data fmt) p.data

