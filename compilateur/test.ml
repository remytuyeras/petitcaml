(* fichier de test du compilateur *)


let rec f n = if n = 0 then 26 else 2+f(n-1)

let d = f 8
let _ = print_string "f(8) = "
let _ = print_int d
let _ = print_newline()

let rec fib n = if n = 0 then 0 else if n = 1 then 1 else fib(n-2)+fib(n-1)

let d = fib 8
let _ = print_string "fib(8) = "
let _ = print_int d
let _ = print_newline()

let _ = print_string "écrire qqch:\n"
let str = read_line();;
let _ = print_string str;;

let ga = print_int;;
let _ = ga 999999999;;
let ixce = print_newline;;
let _ = ixce();;

let f = [5;8;6;5];;

let g = 42::f;;

let rec bob l =
	match l with
	| []-> 0
	| x::h -> x + bob h
	;;


let _ = print_int (bob g);;
let _ = print_newline();;

let test2 x = if x<0 then (fun t -> -t) else (fun t -> t) ;;

let test3 = (test2 5) 8 + (test2 (-7)) 4

let _ = print_int test3
let _ = print_newline();;

let test4 = (4,("k",5,(8,9,(7,()),7),"p",[]),0);;

let (a1,(a2,a3,a4,a5,a6),a7) = test4;;

let (b1,b2,b3,b4) = a4;;

let _ = let _ = print_int b4 in print_newline() ;;
let _ = let _ = print_string a2 in print_newline() ;;

let f = let g = let k = 4 in k+5 in g*2;;

let _ = print_int f
let _ = print_newline();;

let rec x x = x

let _ = print_int (x 5)

let _ = print_newline()
let _ = print_string "donner un nombre:\n"
let d = read_int();;

let _ = print_int d

let _ = print_newline()

