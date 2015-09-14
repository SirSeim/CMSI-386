(* Name: Joshua Kuroda
   Email: jkuroda@lion.lmu.du
   Student ID: 965155965

   Others With Whom I Discussed Things:
   Rodrigo Seim, Adrian Lu, Nick Soffa, Trixie Roque

   Other Resources I Consulted:
   ocaml.org, ocaml documentation, ocamlpro.com
   
*)

(* NOTE: for full credit, add unit tests for each problem.  You should
   define enough unit tests to provide full test coverage for your
   code; each subexpression should be evaluated for at least one
   test.

   Use OCaml's built-in assert function to define unit tests.
  
   run `ocaml hw1.ml` to typecheck and run all tests.
 *)

let _ = assert (1 = 1)
let _ = assert (not (1 = 0))

(* Problem 1
Write a function to compute the nth Fibonacci number, where
the 0th Fibonacci number is 0, the 1st is 1, and the nth for n > 1 is
the sum of the (n-1)st and (n-2)nd Fibonacci numbers.
 *)

let rec fib (n:int) : int =
  if n < 2 then n else fib (n-1) + fib (n-2)
let _ = assert (fib 0 = 0)
let _ = assert (fib 1 = 1)
let _ = assert (fib 2 = 1)

(* Problem 2	       
Write a function clone of type 'a * int -> 'a list.  The function
takes an item e and a nonnegative integer n and returns a list
containing n copies of e.  
 *)

let rec clone ((e, n) : 'a * int) : 'a list =
  if n < 1 then [e] else e :: clone (e, n - 1)

let _ = assert (clone("bread", 1) = ["bread"; "bread"])
let _ = assert (clone("bread", 0) = ["bread"])
let _ = assert (clone("bread", 4) = ["bread"; "bread"; "bread"; "bread"; "bread"])

(* Problem 3
Write a recursive function to get the number of occurrences of an
element in a list. For example, there are 0 occurrences of 5 in [1;2;3].
There are 2 occurrences of 5 in [1;5;5;0].
 *)

let rec count ((value, liste) : ('a * 'a list)) : int =
  match liste with
    | [] -> 0
    | [dernier] when dernier = value  -> 1
    | hd::tl when hd = value  -> 1 + count(value,  tl)
    | hd::tl -> count(value,  tl)

let _ = assert (count("bread", ["bread"; "bread"; "bread"]) = 3)
let _ = assert (count("bread", []) = 0)
let _ = assert (count("bread", ["bread"; "BREAD"; "bread"]) = 2)
let _ = assert (count("bread", ["bread"]) = 1)
let _ = assert (count("bread", ["slice"; "loaf"; "buns"]) = 0)

(* Problem 4
Write a function that appends one list to the front of another.    
 *)

(*
let rec append ((l1,l2) : ('a list * 'a list)) : 'a list =
  TODO
	
(* Problem 5
Use append to write a recursive function that reverses the elements in
a list.
 *)

let rec reverse (l : 'a list) : 'a list =
  TODO
*)
	
(* Problem 6
Write a function "tails" of type 'a list -> 'a list list that takes a
list and returns a list of lists containing the original list along
with all tails of the list, from longest to shortest.
 *)	       

let rec tails (liste : 'a list) : 'a list list = 
  match liste with
    | [] -> [[]]
    | [dernier] -> [[dernier]; []]
    | hd::tl -> liste::(tails(tl))

let _ = assert (tails [1;2;3] = [[1;2;3];[2;3];[3];[]])
let _ = assert (tails [] = [[]])
let _ = assert (tails [1;2;3;4] = [[1;2;3;4];[2;3;4];[3;4];[4];[]])

(* Problem 7
Write a function split of type 'a list -> 'a list * 'a list that
separates out those elements of the given list in odd positions (that
is, the first, third, fifth, etc.) from those in even positions (that
is, the second, fourth, etc.). 
 *)	       

(*
let rec split (liste : 'a list) : 'a list * 'a list =
  match liste with
    | [] -> ([] [])
    | [dernier] -> ([dernier] [])
    | fst:snd:tl -> 

let _ = assert (split [1;2;3;4] = ([1;3], [2;4]))
*)

(* Problem 8
Flatten a list of lists.
 *)

let rec flatten (liste: 'a list list) : 'a list =
  match liste with
    | ([]) -> []
    | ([dernier]) -> dernier
    | (hd::tl) -> hd@(flatten(tl))

let _ = assert (flatten [[2]; []; [3;4]] = [2; 3; 4])
let _ = assert (flatten [[]] = [])
let _ = assert (flatten [[8]] = [8])
let _ = assert (flatten [[2]; [7;8;9]; [3;4]] = [2; 7; 8; 9; 3; 4])
					   
(* Problem 9
Write a function to return the last element of a list. To deal with
the case when the list is empty, the function should return a value of
the built-in option type, defined as follows:

type 'a option = None | Some of 'a
 *)

let rec last (liste: 'a list) : 'a option =
  match liste with
    | [] -> None
    | [dernier] -> Some dernier
    | hd::tl -> last(tl)

let _ = assert (last [] = None)
let _ = assert (last [1;3;2] = Some 2)	
let _ = assert (last [1;3;2;7;8] = Some 8)
let _ = assert (last [11] = Some 11)

(* Problem 10
Write a recursive function to return the longest prefix of a list --
another list containing all but the last element. For example, the
longest prefix of [1;2;3;4;5] is [1;2;3;4]
 *)

let rec longestPrefix (liste : 'a list) : 'a list =
  match liste with
    | [] -> []
    | [dernier] -> []
    | hd::tl -> hd::(longestPrefix(tl))

let _ = assert (longestPrefix [] = [])
let _ = assert (longestPrefix [1;2;3;4;5] = [1;2;3;4])
let _ = assert (longestPrefix [1] = [])
let _ = assert (longestPrefix [1;2] = [1])
		   
(* Problem 11
Write a recursive function that checks whether a list is a
palindrome. A palindrome reads the same forward or backward;
e.g. ["x"; "a"; "m"; "a"; "x"]. Hint: use last and longestPrefix.
 *)
		   
let rec palindrome (liste : 'a list) : bool =
  match liste with
    | [] -> true
    | [dernier] -> true
    | hd::tl when last(tl) = Some hd -> palindrome(longestPrefix(tl))
    | _ -> false

let _ = assert (palindrome [] = true)
let _ = assert (palindrome [5; 4; 3; 2; 1; 2; 3; 4; 5] = true)
let _ = assert (palindrome [4; 2; 5; 1] = false)
let _ = assert (palindrome ["bread"] = true)

(* Problem 12
The naive implementation of fib is wildly inefficient, because it does
a ton of redundant computation.  Perhaps surprisingly, we can make
things much more efficient by building a list of the first n Fibonacci
numbers. Write a function fibsFrom that takes a nonnegative number n
and returns a list of the first n Fibonacci numbers in reverse order
(i.e., from the nth to the 0th).  Recall that the 0th Fibonacci number
is 0, the 1st is 1, and the nth for n > 1 is the sum of the (n-1)st
and (n-2)nd Fibonacci numbers.  You should implement fibsFrom without
writing any helper functions.  A call like (fibsFrom 50) should be
noticeably faster than (fib 50).  Hint: Your function should make only
one recursive call.
 *)

(*
let rec fibsFrom (n:int) : int list =
  TODO

let _ = assert (fibsFrom 1 = [1;0])
		   
(* Problem 13
The naive algorithm for reversing a list takes time that is quadratic
in the size of the argument list.  In this problem, you will implement
a more efficient algorithm for reversing a list: your solution should
only take linear time. Call this function fastRev. The key to fastRev
is that it builds the reversed list as we recurse over the input list,
rather than as we return from each recursive call.  This is similar to
how an iterative version of list reversal, as implemented in a
language like C, would naturally work.

To get the right behavior, your fastRev function should use a local
helper function revHelper to do most of the work.  The helper function
should take two arguments: (1) the suffix of the input list that
remains to be reversed; (2) the reversal of the first part of the
input list.  The helper function should return the complete reversed
list.  Therefore the reversal of an input list l can be performed via
the invocation revHelper(l, []).  I've already provided this setup for
you, so all you have to do is provide the implementation of revHelper
(which is defined as a nested function within fastRev) and invoke it
as listed above.  The call (fastRev (clone(0, 10000))) should be
noticeably faster than (reverse (clone(0, 10000))).
 *)
					   
let fastRev (l : 'a list) : 'a list =
  let rec revHelper (remain, sofar) =
	TODO
in revHelper(l, [])
					   
(* Problem 14
Strings in OCaml do not support pattern matching very well, so it is
sometimes necessary to convert them to something that we can match on
more easily: lists of characters.  Using OCaml's built-in functions
String.get and String.length, write a function chars that converts a
string to a char list.
 *)

let _ = assert (String.get "asdf" 0 = 'a')
let _ = assert (String.length "asdf" = 4)	       

let chars (s:string) : char list =
  TODO

let _ = assert (chars "asdf" = ['a';'s';'d';'f'])
	
(* Problem 15
Convert a list of digits (numbers between 0 and 9) into an integer.
 *)

let int_of_digits (ds : int list) : int =
  TODO

let _ = assert (int_of_digits [1;2;3] = 123)
let _ = assert (int_of_digits [0;1;0] = 10)

*)