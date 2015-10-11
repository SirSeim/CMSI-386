(*Name: Adrian Lu
    Email: alu5@lion.lmu.edu
    Student ID: 977089943
    Others With Whom I Discussed Things: Edward Bramanti, Rodrigo Seim, Lil B, Matthew Brown

    Other Resources I Consulted: Stack Overflow, Google(?), realworldocaml.org
   


    NOTE: for full credit, add unit tests for each problem.  You should
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
the sum of the (n-1)st and (n-2)nd Fibonacci numbers. *)

let rec fib (n : int) : int =
    match n with
    | a when a < 0 -> invalid_arg ("I can only perform this function on nonnegative integers.")
    | i when i < 2 -> n
    | _ -> fib(n - 1) + fib(n - 2)



(* How do I get this assert to pass? The invalid_arg works properly, but don't know what to say in my assertion to simply make this pass through.
    
let _ = assert (Invalid_argument "I can only perform this function on nonnegative integers." = fib(-1)) *)
let _ = assert (0 = fib(0))
let _ = assert (1 = fib(1))
let _ = assert (1 = fib(2))
let _ = assert (233 = fib(13))

(* Problem 2           
Write a function clone of type 'a * int -> 'a list.  The function
takes an item e and a nonnegative integer n and returns a list
containing n copies of e. *)

let rec clone ((e, n) : 'a * int) : 'a list =
    match n with
    | 0 -> []
    | _ -> e :: clone(e, n - 1)

let _ = assert ([] = clone("test", 0))
let _ = assert (["test"] = clone("test", 1))
let _ = assert (["test"; "test"] = clone("test", 2))
let _ = assert ([] = clone(1234, 0))
let _ = assert ([1234] = clone(1234, 1))
let _ = assert ([1234; 1234] = clone(1234, 2))
let _ = assert ([] = clone('z', 0))
let _ = assert (['z'] = clone('z', 1))
let _ = assert (['z'; 'z'] = clone('z', 2))

(* Problem 3
Write a recursive function to get the number of occurrences of an
element in a list. For example, there are 0 occurrences of 5 in [1; 2; 3].
There are 2 occurrences of 5 in [1; 5; 5; 0]. *)

let rec count ((v, l) : ('a * 'a list)) : int =
    match l with
    | [] -> 0
    | hd::tl -> if hd = v then
                    1 + count(v,tl)
                else 
                    count(v,tl)

let _ = assert(0 = count("yes", []))
let _ = assert(0 = count("yes", ["no"; "no"; "no"; "no"; "no"]))
let _ = assert(1 = count("yes", ["yes"]))
let _ = assert(1 = count("yes", ["yes"; "no"; "no"]))
let _ = assert(2 = count("yes", ["yes"; "yes"; "no"]))

(* Problem 4
Write a function that appends one list to the front of another. *)

let rec append ((l1, l2) : ('a list * 'a list)) : 'a list =
    match l1 with
    | [] -> l2
    | hd::tl -> let l3 = hd::append(tl, l2) in
                l3

let _ = assert([] = append([],[]))
let _ = assert(["yes"; "no"; "maybe"; "so"] = append(["yes"; "no"],["maybe"; "so"]))
let _ = assert([1; 2; 3; 4; 5; 6; 7; 8; 9; 10] = append([1; 2; 3; 4; 5],[6; 7; 8; 9; 10]))
    
(* Problem 5
Use append to write a recursive function that reverses the elements in
a list. *)

let rec reverse (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | hd::tl -> append(reverse(tl), [hd]) 

let _ = assert([] = reverse([]))
let _ = assert(['a'] = reverse(['a']))
let _ = assert(['a'; 'b'; 'a'] = reverse(['a'; 'b'; 'a']))
let _ = assert(['c'; 'b'; 'a'] = reverse(['a'; 'b'; 'c']))
    
(* Problem 6
Write a function "tails" of type 'a list -> 'a list list that takes a
list and returns a list of lists containing the original list along
with all tails of the list, from longest to shortest. *)        

let rec tails (ls : 'a list) : 'a list list =
    match ls with
    | [] -> [[]]
    | hd::tl -> ls :: tails(tl)

let _ = assert(tails [1; 2; 3] = [[1; 2; 3]; [2; 3]; [3]; []])
let _ = assert(tails [] = [[]])
let _ = assert(tails [6; 9; 4; 2; 0] = [[6; 9; 4; 2; 0]; [9; 4; 2; 0]; [4; 2; 0]; [2; 0]; [0]; []])

(* Problem 7
Write a function split of type 'a list -> 'a list * 'a list that
separates out those elements of the given list in odd positions (that
is, the first, third, fifth, etc.) from those in even positions (that
is, the second, fourth, etc.). *)        

let rec split (l1 : 'a list) : ('a list * 'a list) =
    match l1 with
    | [] -> [], []
    | [e] -> [e], []
    | [e1; e2] -> [e1], [e2]
    | hd::md::tl -> let l2, l3 = split(tl) in
        (hd::l2, md::l3)

let _ = assert(split [1; 2; 3; 4] = ([1; 3], [2; 4]))
let _ = assert(split [] = ([], []))
let _ = assert(split [1] = ([1], []))
let _ = assert(split [1; 2] = ([1], [2]))

(* Problem 8
Flatten a list of lists. *)

let rec flatten (l: 'a list list) : 'a list =
    match l with
    | [] -> []
    | hd::tl -> append(hd, flatten(tl))

let _ = assert(flatten [[2]; []; [3; 4]] = [2; 3; 4])
let _ = assert(flatten [[]] = [])
let _ = assert(flatten [[6]; [9]; [42]; [0]] = [6; 9; 42; 0])

(* Problem 9
Write a function to return the last element of a list. To deal with
the case when the list is empty, the function should return a value of
the built-in option type, defined as follows:

type 'a option = None | Some of 'a *)

let rec last (l: 'a list) : 'a option =
    match l with
    | [] -> None
    | [e] -> Some e
    | hd::tl -> last(tl)

let _ = assert(last [] = None)
let _ = assert(last [1; 3; 2] = Some 2)
let _ = assert(last [6; 9; 4; 20] = Some 20)

(* Problem 10
Write a recursive function to return the longest prefix of a list --
another list containing all but the last element. For example, the
longest prefix of [1; 2; 3; 4; 5] is [1; 2; 3; 4] *)

let rec longestPrefix (l : 'a list) : 'a list =
    match l with
    | [] -> []
    | [e] -> []
    | hd::tl -> hd::longestPrefix(tl)

let _ = assert(longestPrefix [] = [])
let _ = assert(longestPrefix ['e'] = [])
let _ = assert(longestPrefix [1; 2; 3; 4; 5] = [1; 2; 3; 4])
           
(* Problem 11
Write a recursive function that checks whether a list is a
palindrome. A palindrome reads the same forward or backward;
e.g. ["x"; "a"; "m"; "a"; "x"]. Hint: use last and longestPrefix. *)
           
let rec palindrome (l : 'a list) : bool =
    match l with
    | [] -> true
    | [e] -> true
    | hd::tl when last(tl) = Some hd -> palindrome(longestPrefix(tl))
    | _ -> false

let _ = assert (palindrome([]))
let _ = assert (palindrome([1]))
let _ = assert (palindrome(["x"; "a"; "m"; "a"; "x"]))
let _ = assert (palindrome(["r"; "a"; "c"; "e"; "c"; "a"; "r"]))
let _ = assert (not(palindrome(["x"; "a"; "m"; "a"])))
let _ = assert (not(palindrome(["r"; "a"; "c"; "e"; "c"; "a"])))

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
one recursive call. *)

let rec fibsFrom (n:int) : int list =
    match n with
    | a when a < 0 -> invalid_arg ("I can only perform this function on nonnegative integers.")
    | 0 -> [0]
    | 1 -> [1; 0]
    | _ -> let ls = fibsFrom(n - 1) in
        (List.hd(ls) + List.hd(List.tl(ls))) :: ls

let _ = assert (fibsFrom 0 = [0])
let _ = assert (fibsFrom 1 = [1; 0])
let _ = assert (fibsFrom 7 = [13; 8; 5; 3; 2; 1; 1; 0])
           
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
noticeably faster than (reverse (clone(0, 10000))). *)
                       
let fastRev (l : 'a list) : 'a list =
    let rec revHelper (remain, sofar) =
        match remain with
        | [] -> sofar
        | hd::tl -> revHelper(tl, hd::sofar)
    in revHelper(l, [])

let _ = assert (fastRev([]) = [])
let _ = assert (fastRev([69]) = [69])
let _ = assert (fastRev([1; 2; 3; 4; 5]) = [5; 4; 3; 2; 1])
                       
(* Problem 14
Strings in OCaml do not support pattern matching very well, so it is
sometimes necessary to convert them to something that we can match on
more easily: lists of characters.  Using OCaml's built-in functions
String.get and String.length, write a function chars that converts a
string to a char list.
 *)

let _ = assert (String.get "asdf" 0 = 'a')
let _ = assert (String.length "asdf" = 4)          

let chars (s : string) : char list =
    let rec tolist l ls =
        if l < 0 then
            ls
        else tolist (l - 1) (String.get(s) l :: ls) in
            tolist (String.length(s) - 1) []

let _ = assert (chars "" = [])
let _ = assert (chars "asdf" = ['a'; 's'; 'd'; 'f'])
let _ = assert (chars "abc" = ['a'; 'b'; 'c'])
    
(* Problem 15
Convert a list of digits (numbers between 0 and 9) into an integer.

Thought:
[1; 2; 3; 4; 5]
Take each list element, and multiply it by 10 to the power of its inverse position in the list
10000 + 2000 + 300 + 40 + 5 *)


let rec pow (a1, a2 : int * int) : int =
    match a2 with
    | 0 -> 1
    | 1 -> a1
    | _ -> a1 * pow(a1, a2 - 1)

let _ = assert(25 = pow(5,2))

let rec int_of_digits (ds : int list) : int =
    match ds with
    | [] -> 0
    | hd::tl -> (hd * pow(10, (List.length ds) - 1)) + int_of_digits(tl)

let _ = assert (int_of_digits [] = 0)
let _ = assert (int_of_digits [0] = 0)
let _ = assert (int_of_digits [1; 2; 3] = 123)
let _ = assert (int_of_digits [0; 1; 0] = 10)
let _ = assert (int_of_digits [0; 0; 0] = 0)
let _ = assert (int_of_digits [0; 0; 0; 1] = 1)
let _ = assert (int_of_digits [1; 0; 0; 0; 0; 1] = 100001)
let _ = assert (int_of_digits [6; 9] = 69)
let _ = assert (int_of_digits [4; 2; 0] = 420)