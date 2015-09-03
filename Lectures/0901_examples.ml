1 * 3;;
(* - : int = 3 *)

let x = 42+ 51 * 8;;
(* val x : int = 450 *)

let y = 1 * 4 + 6;;
(* val y : int = 10 *)

let z = x * y;;
(* val z : int = 4500 *)

x * y;;
(* - : int = 4500 *)

let double x = x * 2;;
(* val double : int -> int = <fun> *)

double 5;;
(* - : int = 10 *)

double 10 + 10;;
(* - : int = 30 *)

double (10 + 10);;
(* - : int = 40 *)

(* hello, world! *)
"hello, world";;
(* - : string = "hello, world" *)

let str = "hello, world";;
(* val str : string = "hello, world"
str *)
(* - : string = "hello, world" *)

double 10 + 10;;
(* - : int = 30 *)

(double 10) + 10;;
(* - : int = 30 *)

let quadruple y = double y + double y;;
(* val quadruple : int -> int = <fun>
quadruple 10;; *)
(* - : int = 40 *)

let quadruple y = double double y;;

(* Characters 18-24:
  let quadruple y = double double y;;
                    ^^^^^^
Error: This function has type int -> int
       It is applied to too many arguments; maybe you forgot a `;'. *)

let quadruple y = double (double y);;
(* val quadruple : int -> int = <fun>
quadruple 10;; *)
(* - : int = 40 *)

let rec factorial n = if n=0 then 1 else n * factorial (n-1);;
(* val factorial : int -> int = <fun>
factorial 5;; *)
(* - : int = 120 *)
(* general fom : if <exp> then <exp> else <exp> *)

5 == 3;;
(* - : bool = false *)

let rec factorial n =
  match n with
  | 0 -> 1
  | _ -> n * factorial (n-1)
  ;;
(* val factorial : int -> int = <fun>
[];; *)
(* - : 'a list = [] *)

[1;2;3];;
(* - : int list = [1; 2; 3] *)

["hi"; "bye"];;
(* - : string list = ["hi"; "bye"] *)

let (l : int list) = [1;2;3];;
(* val l : int list = [1; 2; 3] *)

let ls = [[], [1], [""]];;
(* val ls : ('a list * int list * string list) list = [([], [1], [""])] *)

let ls = []::[1]::[""]::[];;

(* Characters 19-21:
  let ls = []::[1]::[""]::[];;
                     ^^
Error: This expression has type string but an expression was expected of type int *)

let ls = 1::""::[];;

(* Characters 12-14:
  let ls = 1::""::[];;
              ^^
Error: This expression has type string but an expression was expected of type int *)

let rec factorial n =
  match n with
  | 0 -> 1
  | _ -> n * factorial (n-1)
  
let msg str =
  match str with
  | "hello" -> "world"
  | "goodbye" -> "see ya"
  | _ -> "what?"