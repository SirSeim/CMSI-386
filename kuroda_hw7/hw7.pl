/* Name: Josh Kuroda

   UID: 965155965

   Others With Whom I Discussed Things: Rodrigo Seim, Lauren Konchan

   Other Resources I Consulted: swi-prolog.org
   
*/

/* Rules and advice:

   Your code should never go into an infinite loop.

   Your code need not produce solutions in a particular order, as long
   as it produces all and only correct solutions and produces each
   solution the right number of times.

   Some of these problems are computationally hard (e.g.,
   NP-complete).  For such problems especially, the order in which you
   put subgoals in a rule can make a big difference on running time.
   In general the best strategy is to put the STRONGEST CONSTRAINTS
   EARLIEST, i.e., the constraints that will prune the search space
   the most.

   Use the hw7-tests.pl file to test! Write more tests!
   $ swipl < hw7-tests.pl
 */

/* Problem 1

Define a predicate duplist(L1,L2) that is true if L2 contains every
two copies of each element of L1.

Examples:
$- duplist([1,2,3], [1,1,2,2,3,3]).
true.

$- duplist([a,b,c], X).
X = [a, a, b, b, c, c].

?- duplist(X, [a,a,a,a,c,c,c,c]).
X = [a, a, c, c].
 */
duplist([],[]).

duplist([_a|_b],[_c,_mid|_d]) :- _a = _c, _a = _mid, duplist(_b,_d).

/* Problem 2 

Define a predicate sorted(L) that is true if L a list of numbers in
increasing order.

Use the less-than-or-equal predicate  =<  (note the weird order).

?- 1 =< 2.
true.
?- 2 =< 1.
false.

Do not use any other built-in predicates!

?- sorted([1,2,3,4,5]).
true.

?- sorted([1,3,2,4,5]).
false.

*/
sorted([]).

sorted(_a) :- length(_a, 1).

sorted([_b, _c|_d]) :- _b =< _c, sorted(_d).

/* Problem 3

Define a predicate perm(L1,L2) that is true if L2 is a permutation of L1.

Use the predicate select(X,L1,L2) that is true if L2 can be obtained
by removing one occurrence of X from L1.

$- select(1, [1,2,3], [2,3]).
true.

?- select(1, [1,2,1,3], [1,2,3]).
true.

Hint: By running select backwards, we can insert 1 into different
positions of [a,b,c]:

?- select(1, X, [a,b,c]).
X = [1, a, b, c] ;
X = [a, 1, b, c] ;
X = [a, b, 1, c] ;
X = [a, b, c, 1] ;
false.

Use sorted and perm to define a predicate permsort(L1,L2) that
is true if L2 is a sorted permutation of L1.

*/
select(_a, [_a|_b], _b).
select(_a, [_c|_b], [_c|_d]) :- select(_a, _b, _d).
perm([],[]).
perm([_x|_y], _z) :- perm(_y, _a), select(_x, _z, _a).
permsort(_w, _z) :- sorted(_z), perm(_w, _z).

/* Problem 4

Define a predicate insert(X,L1,L2) that inserts X into the list L1
(assumed to be sorted), with L2 being the resulting list. You do not
have to check that L1 or L2 are sorted.
  Do not use any predicates other than =<.

Definite another version of insert called insertV2. They should be
true for all the same inputs, but their implementations will be quite
different. insertV2 is true if L1 and L2 are sorted, and L2 contains
one more occurrence of X than L1.
  Use only the predicates sorted and select.

Define a predicate insort(L1,L2) that is true if L2 is a sorted
permutation of L1. Use only insert or insertV2.

*/
insert(_a, [_b|_c], [_d|_e]) :- _b =< _a, insert(_a, _c, _e).
insert(_a, _f, [_a|_f]).
insertV2(_a, _f, [_a|_f]).

insertV2(_a, [_b|_c], [_d|_e]) :-
    sorted(_g),
    sorted(_h),
    select(_a, _h, _g),
    _b =< _a, insert(_a, _c, _e).

insort([_j|_k], _h) :- insert(_j, _k, _h), insort(_k, _h).

/* Problem 5

Compare the time it takes prolog to find 1 solution for each of:

?- permsort([5,3,6,2,7,4,5,4,1,2,8,6],L).

vs 

?- insort([5,3,6,2,7,4,5,4,1,2,8,6],L).

Which is faster? Why?

Insort, because permsort is checking every permutation, which increases
    the complexity. 
*/

/* Problem 6 

In this problem, you will write a Prolog program to solve a form of
the "blocks world" problem, which is a famous planning problem from
artificial intelligence.  Imagine you have three stacks of blocks in a
particular configuration, and the goal is to move blocks around so
that the three stacks end up in some other configuration.  You have a
robot that knows how to do two kinds of actions.  First, the robot can
pick up the top block from a stack, as long as that stack is nonempty
and the robot's mechanical hand is free.  Second, the robot can place
a block from its hand on to a stack.

Implement a predicate blocksworld(Start, Actions, Goal). Start and
Goal are lists describing configurations (states) of the world, and
Actions is a list of actions. blocksworld(Start, Actions, Goal) should
be true if the robot can move from the Start state to the Goal state
by following the list of Actions.

We will represent blocks as single-letter atoms like a,b,c, etc.

We will represent a world as a relation world(S1,S2,S3,H) that has
four components: three lists of blocks S1, S2, and S2 that represent
the three stacks, and a component H that represents the contents of
the mechanical hand.  That last component H either contains a single
block or the atom none, to represent the fact that the hand is empty.

Some example configurations of the world:

  world([a,b,c],[],[d],none)   
    - The first stack contains blocks a,b,c (a is at the top).
    - The second stack is empty.
    - The third stack contains the block d.
    - The hand is empty.

  world([],[],[],a)
    - The stacks are all empty.
    - The hand contains the block a.

There are two kinds of actions: pickup(S) and putdown(S). In each
action, S must be one of the atoms stack1, stack2, or stack3, which
identifies which stack to pickup from or putdown to. For example,
pickup(stack1) instructs the robot to pickup from stack1, and
putdown(stack2) instructs it to put down the currently held block on stack2.

First define a predicate perform(Start,Action,Goal), which defines the
effect of a single action on the configuration.  Use this to define
the predicate blocksworld(Start, Actions, Goal).

Once you've defined perform and blocksworld, you can ask for the
solutions:

?- length(Actions,L), blocksworld(world([a,b,c],[],[],none), Actions, world([],[],[a,b,c],none)).

Actions = [pickup(stack1),putdown(stack2),pickup(stack1),putdown(stack2),pickup(stack1),putdown(stack3),pickup(stack2),putdown(stack3),pickup(stack2),putdown(stack3)]
L = 10 ?

Notice how I use length to limit the size of the resulting list of
actions. The effect of this is that Prolog will search for a solution
consisting of 0 actions, then 1 action, then 2 actions, etc.  This is
necessary to do when you test your code, in order to prevent Prolog
from getting stuck down infinite paths (e.g., continually picking up
and putting down the same block).

*/
perform(world([_hand|_b],_st2,_st3,none), pickup(stack1), world(_b,_st2,_st3,_hand)).

perform(world(_st1,_st2,_st3,_hand), putdown(stack1), world([_hand|_st1],_st2,_st3,none)) :- _hand \= none.

perform(world(_st1,[_hand|_b],_st3,none), pickup(stack2), world(_st1,_b,_st3,_hand)).

perform(world(_st1,_st2,_st3,_hand), putdown(stack2), world(_st1,[_hand|_st2],_st3,none)) :- _hand \= none.

perform(world(_st1,_st2,[_hand|_b],none), pickup(stack3), world(_st1,_st2,_b,_hand)).

perform(world(_st1,_st2,_st3,_hand), putdown(stack3), world(_st1,_st2,[_hand|_st3],none)) :- _hand \= none.

blocksworld(_start, [], _goal) :- _start = _goal.

blocksworld(_start, [_hand|_b], _goal) :- perform(_start, _hand, _z), blocksworld(_z, _b, _goal).
