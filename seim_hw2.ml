(* Name: Edward Seim

   UID: 930713935

   Others With Whom I Discussed Things: Josh Kuroda

   Other Resources I Consulted: http://caml.inria.fr/
   
*)

(* For this assignment, you will get practice with higher-order functions
 * in OCaml.  In a few places below, you are required to implement your
 * functions in a particular way, so pay attention to those directives or
 * you will get no credit for the problem.
 *)

(* Do not use any functions defined in other modules, except for List.map,
 * List.filter, List.fold_left, and List.fold_right. 
 *)

let map = List.map
let filter = List.filter
let fold_left = List.fold_left
let fold_right = List.fold_right

(************************************************************************
 * Problem 1: Using higher-order functions.
 *
 * Implement each function below as a single call to one of map, filter,
 * fold_left, or fold_right.
 ************************************************************************)

(* Problem 1a.
   A function that takes a list as input and returns the same list except 
   with all positive integers doubled in value.  Other integers are
   kept but are unchanged.
 *)

let doubleAllPos (l : int list) : int list = 
   map (fun x -> if x > 0 then x*2 else x) l

let _ = assert (doubleAllPos [1;2;-1;4;-3;0] = [2;4;-1;8;-3;0]);;
let _ = assert (doubleAllPos [] = [])
let _ = assert (doubleAllPos [-1] = [-1])
let _ = assert (doubleAllPos [-1;2] = [-1;4])

(* Problem 1b.
   A function that takes a list of pairs and returns a pair of lists.
   The first list contains the first component of each input pair;
   the second list contains the second components.
 *)
   
let unzip (l : ('a * 'b) list) : 'a list * 'b list = 
   fold_left (fun l x -> let rl1, rl2 = l in let t1, t2 = x in (rl1@[t1], rl2@[t2])) ([], []) l

let _ = assert (unzip [] = ([], []))
let _ = assert (unzip [(0,0)] = ([0], [0]))
let _ = assert (unzip [(1,'a');(2,'b')] = ([1;2], ['a';'b']));;

(* Problem 1c.
   Implement the so-called run-length encoding data compression method.
   Consecutive duplicates of elements are encoded as pairs (N, E) where
   N is the number of duplicates of the element E.
 *)

(* let encode : 'a list -> (int * 'a) list = 


let _ = assert (encode ['a';'a';'a';'b';'c';'c'] = [(3,'a');(1,'b');(2,'c')]);; *)

(* Problem 1d
   The function intOfDigits from Homework 1.
 *)

let intOfDigits (l : int list) : int = 
   fold_left (fun n x -> (n*10)+x) 0 l

let _ = assert (intOfDigits [] = 0)
let _ = assert (intOfDigits [0] = 0)
let _ = assert (intOfDigits [5] = 5)
let _ = assert (intOfDigits [5;4] = 54)
let _ = assert (intOfDigits [3;4;5] = 345)

(***********************************************************************
 * Problem 2: Defining higher-order functions.
 ***********************************************************************)

(* Problem 2a.  

   A useful variant of map is the map2 function, which is like map but
   works on two lists instead of one. Note: If either input list is
   empty, then the output list is empty.

   Define map2 function using explicit recursion.

   Do not use any functions from the List module or other modules.
 *)

(* let rec map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list = TODO

let _ = assert (map2 (fun x y -> x*y) [1;2;3] [4;5;6] = [1*4; 2*5; 3*6]);; *)

(* Problem 2b.

   Now implement the zip function, which is the inverse of unzip
   above. zip takes two lists l1 and l2, and returns a list of pairs,
   where the first element of the ith pair is the ith element of l1,
   and the second element of the ith pair is the ith element of l2.

   Implement zip as a function whose entire body is a single call to
   map2.
 *)

(* let zip : 'a list -> 'b list -> ('a * 'b) list = map2 TODO

let _ = assert (zip [1;2] ['a';'b']  = [(1,'a');(2,'b')]);; *)

(* Problem 2c.

   A useful variant of fold_right and fold_left is the foldn function,
   which folds over an integer (assumed to be nonnegative) rather than
   a list. Given a function f, an integer n, and a base case b, foldn
   calls f n times. The input to the first call is the base case b,
   the input to second call is the output of the first call, and so
   on.

   For example, we can define the factorial function as:
   let fact n = foldn (fun x y -> x*y) n 1

   Implement foldn using explicit recursion.
 *)

(* let rec foldn : (int -> 'a -> 'a) -> int -> 'a -> 'a = TODO

let _ = assert (foldn (fun x y -> x*y) 5 1 = 5 * 4 * 3 * 2 * 1);;
let _ = assert (foldn (fun x y -> x-y) 5 1 = 5 - (4 - (3 - (2 - 1))));; *)

(* Problem 2d.
   Implement the clone function from Homework 1 as a single call to
   foldn.
 *)

(* Problem 2e.
   Implement fibsFrom from Homework1 as a single call to foldn.
 *)

(************************************************************************
 * Problem 3: Dictionaries.
 * A dictionary (sometimes also called a map) is a data structure that 
 * associates keys with values (or maps keys to values). A dictionary 
 * supports three main operations:  empty, which returns an empty
 * dictionary; put, which adds a new key-value pair to a given dictionary; 
 * and get, which looks up the value associated with a given key in a
 * given dictionary.  If the given key is already associated with some 
 * value in the dictionary, then put should (conceptually) replace the old
 * key-value pair with the new one.  To handle the case when the given
 * key is not mapped to some value in the dictionary, get will return an
 * option, i.e. either the value None or the value Some v, where v is the
 * value associated with the given key in the dictionary.

 * In this problem we'll explore three different implementations of a 
 * dictionary data structure. It's OK if the types that OCaml infers for some 
 * of these functions are more general than the types we specify. Specifically,
 * the inferred types could use a type variable like 'a in place of a more
 * specific type.
 ************************************************************************)

(* Problem 3a.

   Our first implementation of a dictionary is as an "association
   list", i.e. a list of pairs. Implement empty1, put1, and get1 for
   association lists (we use the suffix 1 to distinguish from other
   implementations below).  As an example of how this representation
   of dictionaries works, the dictionary that maps "hello" to 5 and
   has no other entries is represented as [("hello",5)].

   To get the effect of replacing old entries for a key, put1 should
   simply add new entries to the front of the list, and accordingly
   get1 should return the leftmost value whose associated key matches
   the given key.

   empty1: unit -> ('a * 'b) list
   put1: 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list
   get1: 'a -> ('a * 'b) list -> 'b option
 *)  

(* Problem 3b.

   Our second implementation of a dictionary uses a new datatype 
   "dict2", defined below.

   dict2 is polymorphic in the key and value types, which respectively
   are represented by the type variables 'a and 'b.  For example, the
   dictionary that maps "hello" to 5 and has no other entries would be
   represented as the value Entry("hello", 5, Empty) and
   would have the type (string,int) dict2.

   Implement empty2, put2, and get2 for dict2.  As above, new entries
   should be added to the front of the dictionary, and get2 should
   return the leftmost match.

   empty2: unit -> ('a,'b) dict2
   put2: 'a -> 'b -> ('a,'b) dict2 -> ('a,'b) dict2
   get2: 'a -> ('a,'b) dict2 -> 'b option
 *)  
    
(* type ('a,'b) dict2 = Empty | Entry of 'a * 'b * ('a,'b) dict2 *)
    
(* Problem 3c

   Conceptually, a dictionary is just a function from keys to values.
   Since OCaml has first-class functions, we can choose to represent
   dictionaries as actual functions.  We define the following type:

   type ('a,'b) dict3 = ('a -> 'b option)

   We haven't seen the above syntax before (note that the right-hand
   side just says ('a -> 'b option) rather than something like Foo of
   ('a -> 'b option)).  Here dict3 is a type synonym: it is just a
   shorthand for the given function type rather than a new type.  As
   an example of how this representation of dictionaries works, the
   following dictionary maps "hello" to 5 and has no other entries:

   (function s ->
    match s with
    | "hello" -> Some 5
    | _ -> None)

   One advantage of this representation over the two dictionary
   implementations above is that we can represent infinite-size
   dictionaries.  For instance, the following dictionary maps all
   strings to their length (using the String.length function):

   (function s -> Some(String.length s))

   Implement empty3, put3, and get3 for dict3.  It's OK if the types
   that OCaml infers for these functions use ('a -> 'b option) in
   place of ('a,'b) dict3, since they are synonyms for one another.

   empty3: unit -> ('a,'b) dict3
   put3: 'a -> 'b -> ('a,'b) dict3 -> ('a,'b) dict3
   get3: 'a -> ('a,'b) dict3 -> 'b option
 *)  

(* type ('a,'b) dict3 = ('a -> 'b option) *)

