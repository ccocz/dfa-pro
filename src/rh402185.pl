% @author Resul Hangeldiyev

% Binary search tree with key/value pair
% bst(empty).
% bst(node(L, _, P)) :- bst(L), bst(P).

insertBST(empty, K-V, node(empty, K-V, empty)).
insertBST(node(L, K-V, R), X-_, node(L, K-V, R)) :-
    X == K,
    !.
insertBST(node(L, K-V, R), X-Y, node(L1, K-V, R)) :-
    X @=< K,
    !,
    insertBST(L, X-Y, L1).
insertBST(node(L, K-V, R), X-Y, node(L, K-V, R1)) :-
    X @> K,
    insertBST(R, X-Y, R1).

% Succeeds iff K0 is present in the given BST and has V0 value attached.
getKeyBST(node(_, K-V, _), K0, V) :-
    K == K0,
    !.

getKeyBST(node(L, K-_, _), K0, V1) :-
    K0 @=< K,
    !,
    getKeyBST(L, K0, V1).

getKeyBST(node(_, K-_, R), K0, V1) :-
    K0 @> K,
    getKeyBST(R, K0, V1).

% dfa(TransitionFunction, StartingState, FinalStates)

transitionFunction([]).
transitionFunction([fp(_, _, _) | R]) :- transitionFunction(R).

dfa(TF, _, _) :- transitionFunction(TF).

% Get all states out of transitions, stored in BST.
getStates([], T) :- getStates([], empty, T).
getStates([], T, T).
getStates([fp(S1, _, S2) | Y], A, T) :-
    insertBST(A, S1-1, A0),
    insertBST(A0, S2-1, A1),
    getStates(Y, A1, T).

% Get alphabet out of transitions, stored in BST.
getAlphabet([], T) :- getAlphabet([], empty, T).
getAlphabet([], T, T).
getAlphabet([fp(_, C, _) | Y], A, T) :-
    insertBST(A, C-1, A0),
    getAlphabet(Y, A0, T).

% Get transitions as BST.
getTransitions([], T) :- getTransitions([], empty, T).
getTransitions([], T, T).
getTransitions([fp(S1, C, S2) | Y], A, T) :-
    insertBST(A, (S1-C)-S2, A0),
    getTransitions(Y, A0, T).

% Check if all keys in the given list exist in the given BST.
doAllExist([], _).
doAllExist([X | Y], T) :-
    getKeyBST(T, X, _),
    doAllExist(Y, T).

% DFA(states, alphabet, transition, start, final)

% correct(+Automat, -Reprezentacja)
correct(dfa(TF, SS, FS), dfa(TF, SS, FS)).

% accept(+Automat, ?Słowo)
accept(dfa(_, SS, FS), []) :- member(SS, FS).
accept(dfa(TF, SS, FS), [L | R]) :-
    \+empty(dfa(TF, SS, FS)),
    member(fp(SS, L, NX), TF),
    accept(dfa(TF, NX, FS), R).

% empty(+Automat)
empty(dfa(TF, SS, FS)) :- \+finalPath(dfa(TF, SS, FS), []).

finalPath(dfa(_, SS, FS), _) :- member(SS, FS).
finalPath(dfa(TF, SS, FS), V) :-
    \+member(SS, FS),
    member(fp(SS, _, NX), TF),
    \+member(NX, V),
    finalPath(dfa(TF, NX, FS), [NX | V]).

% equal(+Automat1, +Automat2)
% subsetEq(+Automat1, +Automat2)
