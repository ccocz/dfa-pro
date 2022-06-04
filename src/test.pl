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

:- end_tests(rh402185).
