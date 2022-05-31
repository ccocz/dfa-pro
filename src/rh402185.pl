% @author Resul Hangeldiyev

/*
 * todos:
 * - check if x-y pair representation is valid
 */


insertBST(empty, K-V, node(empty, K-V, empty)).
insertBST(node(L, K-_, R), K-V0, node(L, K-V0, R)).
insertBST(node(L, K-V, R), X-Y, node(L1, K-V, R)) :-
    X @< K,
    insertBST(L, X-Y, L1).
insertBST(node(L, K-V, R), X-Y, node(L, K-V, R1)) :-
    X @> K,
    insertBST(R, X-Y, R1).

% Succeeds iff K0 is present in the given BST and has V0 value attached.
getKeyBST(node(_, K-V, _), K, V).

getKeyBST(node(L, K-_, _), K0, V1) :-
    K0 @< K,
    getKeyBST(L, K0, V1).

getKeyBST(node(_, K-_, R), K0, V1) :-
    K0 @> K,
    getKeyBST(R, K0, V1).

/*getKeyBST(node(_, _, R), K, V) :-
    getKeyBST(R, K, V).*/

% allPresent(List, BST) true iff all keys from list appears in BST.
allPresent([], _).
allPresent([L | R], T) :-
    getKeyBST(T, L, _),
    allPresent(R, T).

% create list out of BST keys
toListBST(T, L) :- toListBST(T, [], L).
toListBST(empty, A, A).
toListBST(node(L, K0-_, R), A, K) :-
  toListBST(R, A, K1),
  toListBST(L, [K0 | K1], K).

% dfa(TransitionFunction, StartingState, FinalStates)

transitionFunction([]).
transitionFunction([fp(_, _, _) | R]) :- transitionFunction(R).

dfa(TF, _, _) :- transitionFunction(TF).

% Get all states out of transitions, stored in BST.
% Initially all states have 0 value assigned, 1 and 2 represents
% start and final states respectively.
getStates(L, T) :- getStates(L, empty, T).
getStates([], T, T).
getStates([fp(S1, _, S2) | Y], A, T) :-
    insertBST(A, S1-0, A0),
    insertBST(A0, S2-0, A1),
    getStates(Y, A1, T).

% Get alphabet out of transitions, stored in BST.
getAlphabet(L, T) :- getAlphabet(L, empty, T).
getAlphabet([], T, T).
getAlphabet([fp(_, C, _) | Y], A, T) :-
    insertBST(A, C-C, A0),
    getAlphabet(Y, A0, T).

% Get transitions as BST.
getTransitions(L, T) :- getTransitions(L, empty, T).
getTransitions([], T, T).
getTransitions([fp(S1, C, S2) | Y], A, T) :-
    \+ getKeyBST(A, S1-C, _),
    insertBST(A, (S1-C)-S2, A0),
    getTransitions(Y, A0, T).

% Check if all keys in the given list exist in the given BST.
doAllExist([], _).
doAllExist([X | Y], T) :-    
    getKeyBST(T, X, _),
    doAllExist(Y, T).

% cartesianProduct(BST_X, BST_Y, List) is true iff 
% [keys of BST_X] * [keys of BST_Y] = List.
cartesianProduct(X, Y, Z) :- cartesianProduct(X, Y, [], Z).
cartesianProduct(empty, _, A, A).
cartesianProduct(_, empty, A, A) :- !.
cartesianProduct(node(L, K-_, R), Y, A, Z) :-
    prependAll(K, Y, A, LY),
    cartesianProduct(L, Y, LY, Z0),
    cartesianProduct(R, Y, Z0, Z).

% Create pairs of given element with all nodes of given BST.
prependAll(X, Y, Z) :- prependAll(X, Y, [], Z).
prependAll(_, empty, A, A).
prependAll(X, node(L, K-_, R), A, Z) :-
    prependAll(X, L, [(X-K) | A], Z0),
    prependAll(X, R, Z0, Z).


% checkFullGraph(States, Alphabet, Transitions) true iff single outgoing
% edge for each letter in alphabet.
checkFullGraph(S, A, T) :-
    cartesianProduct(S, A, P),
    allPresent(P, T).

% Insert all final states with corresponding value.
insertAllBST([], T, T).
insertAllBST([L | R], T, Z) :-
    insertBST(T, L-2, T0),
    insertAllBST(R, T0, Z).

% dfaInternal(states, alphabet, transitions, start, final)

% correct(+Automat, -Reprezentacja)
correct(dfa(TF, SS, FS), dfaInternal(S1, T, AL)) :-
    getTransitions(TF, T),
    getStates(TF, S),
    getAlphabet(TF, A),
    toListBST(A, AL),
    write("oki "), write(SS), nl,
    checkFullGraph(S, A, T),
    getKeyBST(S, SS, _),
    allPresent(FS, S),
    %    insertBST(S, SS-1, S0),
    insertAllBST(FS, S, S1).

accept(dfa(TF, SS, FS), []) :-
    correct(dfa(TF, SS, FS), dfaInternal(S, _, _)),
    getKeyBST(S, SS, 2).

% accept(+Automat, ?Słowo)
accept(dfa(TF, SS, FS), [L | R]) :-
    correct(dfa(TF, SS, FS), dfaInternal(S, T, A)),
    member(L, A),
    (isAlive(SS, S, T, A) ->
        getKeyBST(T, SS-L, N),
        accept(dfa(TF, N, FS), R);
        !, fail
    ).

isAlive(C, S, T, A) :-
    findFinalPath(C, S, T, A, []).

% empty(+Automat)
empty(dfa(TF, SS, FS)) :- \+finalPath(dfa(TF, SS, FS)).

finalPath(dfa(TF, SS, FS)) :-
    correct(dfa(TF, SS, FS), dfaInternal(S, T, A)),
    findFinalPath(SS, S, T, A, []).

findFinalPath(C, S, _, _, _) :-
    getKeyBST(S, C, 2),
    !.

findFinalPath(C, S, T, A, V) :-
    member(NX, A),
    getKeyBST(T, C-NX, NS),
    \+ member(NS, V),
    findFinalPath(NS, S, T, A, [NS | V]).

% equal(+Automat1, +Automat2)
equal(dfa(TF_A, SS_A, FS_A), dfa(TF_B, SS_B, FS_B)) :-
    correct(dfa(TF_A, SS_A, FS_A), dfaInternal(_, T_A, A_A)),
    correct(dfa(TF_B, SS_B, FS_B), dfaInternal(_, T_B, A_B)),
    equalList(A_A, A_B),
    construct(T_A, T_B, A_A, [fp(0,1,(SS_A-SS_B))], TM, []),
    %getTransitions(TM, X),
    %write(X).
    correct(dfa(TM, SS_A-SS_B, [SS_A-SS_B]), X),
    write(X).

equalList(L1, L2) :- 
    \+ (member(X, L1), \+ member(X, L2)),
    \+ (member(X, L2), \+ member(X, L1)).
% Given starting nodes, transitions and alphabet merge two DFAs by
% using product construction.
% fp(S1, C, S2)


getAll(_, _, _, _, [], []).
getAll(SA, SB, TA, TB, [X | A], [fp((SA-SB), X, (NA-NB)) | L]) :-
     getKeyBST(TA, SA-X, NA),
     getKeyBST(TB, SB-X, NB),
     getAll(SA, SB, TA, TB, A, L).

construct(TA, TB, A, SA, TM, V) :- 
    productConstruction(TA, TB, A, SA, TM, [], V).

productConstruction(_, _, _, [], TM, TM, _).

productConstruction(%SA,
 %SB,
                     TA,
                     TB,
                     A,
                     [fp(_,_,(SXA-SXB)) | K],
                     TM,
                     TMA,
                     V) :-
                         %write(V),nl,
     member(SXA-SXB, V),
     !,
     %write(K),write(" hell "), write(TM), nl,
     productConstruction(TA, TB, A, K, TM, TMA, V).

productConstruction(%SA,
%SB,
                    TA, 
                    TB, 
                    A,
                    [fp(_,_,(SXA-SXB)) | K],
                    TM,
                    TMA,
                    V) :-
    /*member(C, A),
    getKeyBST(TA, SXA-C, NA),
    getKeyBST(TB, SXB-C, NB),
\+ member(NA-NB, V),*/

    %write(SXA-SXB),nl,
    write("hell: "), write(SXA-SXB),nl,
    getAll(SXA, SXB, TA, TB, A, N),
    append(K, N, K1),
    %write(SXA-SXB), write(" "), write(NA-NB), write(" "), write(V), nl,
    %write(K1), write('\n'),
    %    write(N),nl,
    append(N, TMA, TMAN),
    productConstruction(TA, TB, A, K1, TM, TMAN, [SXA-SXB | V]).

% subsetEq(+Automat1, +Automat2)

% example(IdentyfikatorAutomatu, Automat)
example(a11, dfa([fp(1,a,1),fp(1,b,2),fp(2,a,2),fp(2,b,1)], 1, [2,1])).
example(a12, dfa([fp(x,a,y),fp(x,b,x),fp(y,a,x),fp(y,b,x)], x, [x,y])).
example(a2, dfa([fp(1,a,2),fp(2,b,1),fp(1,b,3),fp(2,a,3), fp(3,b,3),fp(3,a,3)], 1, [1])).
example(a3, dfa([fp(0,a,1),fp(1,a,0)], 0, [0])).
example(a4, dfa([fp(x,a,y),fp(y,a,z),fp(z,a,x)], x, [x])).
example(a5, dfa([fp(x,a,y),fp(y,a,z),fp(z,a,zz),fp(zz,a,x)], x, [x])).
example(a6, dfa([fp(1,a,1),fp(1,b,2),fp(2,a,2),fp(2,b,1)], 1, [])).
example(a7, dfa([fp(1,a,1),fp(1,b,2),fp(2,a,2),fp(2,b,1), fp(3,b,3),fp(3,a,3)], 1, [3])).
% bad ones
example(b1, dfa([fp(1,a,1),fp(1,a,1)], 1, [])).
example(b2, dfa([fp(1,a,1),fp(1,a,2)], 1, [])).
example(b3, dfa([fp(1,a,2)], 1, [])).
example(b4, dfa([fp(1,a,1)], 2, [])).
example(b5, dfa([fp(1,a,1)], 1, [1,2])).
example(b6, dfa([], [], [])).
