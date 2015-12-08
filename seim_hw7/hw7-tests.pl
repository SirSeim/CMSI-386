% usage: $ swipl < hw7-tests.pl

consult(hw7).

% Problem 1 tests
duplist([1,2,3], [1,1,2,2,3,3]).

duplist([1,1,3,2], [1,1,1,1,3,3,2,2]).

% Problem 2 tests

sorted([1,2,3,4,5]).

% Problem 3 tests

perm([1,5,3,2,4], [1,2,3,4,5]).

permsort([1,5,3,2,4], [1,2,3,4,5]).

% Problem 4 tests

insert(3, [1,2,4,5], [1,2,3,4,5]).

% insert does not require input lists to be sorted.
insert(3, [2,1,5,4], [2,1,3,5,4]).

insertV2(3, [1,2,4,5], [1,2,3,4,5]).

insort([1,5,3,2,4], [1,2,3,4,5]).

% Problem 5 tests

perform(world([x],[],[],none),             % initial world
        pickup(stack1),                    % action
        world([],[],[],x)).                % final world

perform(world([],[],[],x),                 % initial world
        putdown(stack2),                   % action
        world([],[x],[],none)).            % final world

blocksworld(world([x],[],[],none),              % initial world
            [pickup(stack1),putdown(stack2)],   % actions
            world([],[x],[],none)).             % final world

