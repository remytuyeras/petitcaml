
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
| Jr 			of register
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

val print_program : Format.formatter -> program -> unit;;
