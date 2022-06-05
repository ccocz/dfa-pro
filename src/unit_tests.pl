% example(ID, Automata)
example(a11, dfa([fp(1,a,1),fp(1,b,2),fp(2,a,2),fp(2,b,1)], 1, [2,1])).
example(a12, dfa([fp(x,a,y),fp(x,b,x),fp(y,a,x),fp(y,b,x)], x, [x,y])).
example(a2, 
    dfa([fp(1,a,2),
        fp(2,b,1),fp(1,b,3),fp(2,a,3), fp(3,b,3),fp(3,a,3)], 1, [1])).
example(a3, dfa([fp(0,a,1),fp(1,a,0)], 0, [0])).
example(a4, dfa([fp(x,a,y),fp(y,a,z),fp(z,a,x)], x, [x])).
example(a5, dfa([fp(x,a,y),fp(y,a,z),fp(z,a,zz),fp(zz,a,x)], x, [x])).
example(a6, dfa([fp(1,a,1),fp(1,b,2),fp(2,a,2),fp(2,b,1)], 1, [])).
example(a7, dfa([fp(1,a,1),fp(1,b,2),fp(2,a,2),fp(2,b,1), 
    fp(3,b,3),fp(3,a,3)], 1, [3])).

% bad ones
example(b1, dfa([fp(1,a,1),fp(1,a,1)], 1, [])).
example(b2, dfa([fp(1,a,1),fp(1,a,2)], 1, [])).
example(b3, dfa([fp(1,a,2)], 1, [])).
example(b4, dfa([fp(1,a,1)], 2, [])).
example(b5, dfa([fp(1,a,1)], 1, [1,2])).
example(b6, dfa([], [], [])).

% custom
example(c1, dfa([fp(0, b, 1), 
                 fp(0, a, 2),
                 fp(1, a, 1),
                 fp(1, b, 2),
                 fp(2, a, 2),
                 fp(2, b, 2)], 0, [1, 2])).

example(c2, dfa([fp(0, a, 1), 
                 fp(0, b, 1),
                 fp(1, a, 1),
                 fp(1, b, 1)],
                 0,
                 [1]
                )).

example(c3, dfa([fp(0, a, 1),
                 fp(0, b, 1),
                 fp(1, a, 2),
                 fp(1, b, 2),
                 fp(2, a, 2),
                 fp(2, b, 2)], 0, [1])).

example(c4, dfa([fp(0, a, 1),
                 fp(0, b, 1),
                 fp(1, a, 2),
                 fp(1, b, 2),
                 fp(2, a, 3),
                 fp(2, b, 4),
                 fp(3, a, 3),
                 fp(3, b, 3),
                 fp(4, a, 3),
                 fp(4, b, 4)], 0, [1, 2])).

example(c5, dfa([fp(1, a, 2), fp(2, a, 2)], 1, [2])).


:- begin_tests(rh402185).

% Success
test(1) :- example(a11, A), example(a12, B), equal(A, B).
test(2) :- example(a2, A), example(a11, B), subsetEq(A, B).
test(3) :- example(a5, A), example(a3, B), subsetEq(A, B).
test(4) :- example(a6, A), empty(A).
test(5) :- example(a7, A), empty(A).
test(6) :- example(a2, A), accept(A, []).
test(7) :- example(a2, A), accept(A, [a,b]).
test(8) :- example(a2, A), accept(A, [a,b,a,b]).

% Failure
test(9) :- \+ (example(b1, A), correct(A, _)).
test(10) :- \+ (example(b2, A), correct(A, _)).
test(11) :- \+ (example(b3, A), correct(A, _)).
test(12) :- \+ (example(b4, A), correct(A, _)).
test(13) :- \+ (example(b5, A), correct(A, _)).

test(14) :- \+ (example(a2, A), empty(A)).
test(15) :- \+ (example(a3, A), example(a4, B), equal(A, B)).
test(16) :- \+ (example(a4, A), example(a3, B), subsetEq(A, B)).
test(17) :- \+ (example(a2, A), accept(A, [a])).

% Custom tests
test(18) :- \+ (example(c1, A), empty(A)).
test(19) :- (example(c1, A), example(c2, B), equal(A, B)).
test(20) :- \+ (example(c1, A), example(a2, B), equal(A, B)).

test(21) :- example(c1, A), example(a11, B), subsetEq(A, B).
test(22) :- example(c1, A), example(a12, B), subsetEq(A, B).

test(23) :-
    findall([X, Y], (example(a11, A), accept(A, [X, Y])), Xs),
    Xs == [[a, a],
           [a, b],
           [b, a],
           [b, b]].

test(24) :-
    findall(X, (example(c3, A), accept(A, X)), Xs),                  
     Xs == [[a], [b]]. 

test(25) :- \+ (example(c3, A), example(c4, B), equal(A, B)).

test(26) :- example(c3, A), example(c4, B), subsetEq(A, B).

test(27) :-
     findall(X, (example(c4, A), accept(A, X)), Xs),
     sort(Xs, Xss),
     Xss == [[a],
            [a, a],
            [a, b],
            [b],
            [b, a],
            [b, b]].

:- end_tests(rh402185).
