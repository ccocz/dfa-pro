% @author Resul Hangeldiyev

/*
 * todos:
 * - check if x-y pair representation is valid
 */

% memberchk, punktu nadbiour
% visited as bst
% correct before all

:- use_module(library(lists)).

insertBST(node(L, K-V, R), X-Y, node(L1, K-V, R)) :-
    X @< K,
    insertBST(L, X-Y, L1).
insertBST(node(L, K-V, R), X-Y, node(L, K-V, R1)) :-
    X @> K,
    insertBST(R, X-Y, R1).

insertBST(node(L, K-_, R), K-V0, node(L, K-V0, R)).
insertBST(empty, K-V, node(empty, K-V, empty)).

initPair(X-Y) :-
    \+ var(X),
    \+ var(Y),
    initPair(X), initPair(Y).

% Succeeds iff K0 is present in the given BST and has V0 value attached.

getKeyBST(node(_, K-V, _), K, V).  

getKeyBST(node(L, K-_, _), K0, V1) :-
    initPair(K0),
    K0 @< K,
    getKeyBST(L, K0, V1).

getKeyBST(node(L, K-_, _), K0, V1) :-
    initPair(K0),
    K0 @> K,
    getKeyBST(L, K0, V1).

getKeyBST(node(_, _, R), K0, V1) :-
    \+ initPair(K0),
    getKeyBST(R, K0, V1).

getKeyBST(node(L, _, _), K0, V1) :-
    \+ initPair(K0),
    getKeyBST(L, K0, V1).

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
% Initially all states have 0 value assigned, value of 1 represents
% final states.
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
cartesianProduct(_, empty, A, A).
cartesianProduct(node(L, K-_, R), Y, A, Z) :-
    prependAll(K, Y, A, LY),
    cartesianProduct(L, Y, LY, Z0),
    cartesianProduct(R, Y, Z0, Z).

/*

% Similar to above predicate, only picked nodes has to meet value constraint.
cartesianProduct(X, Y, XV, YV, Z) :- cartesianProduct(X, Y, XV, YV, [], Z).                     
cartesianProduct(empty, _, _, _, A, A).                                               
cartesianProduct(_, empty, _, _, A, A).
cartesianProduct(node(L, K-V, R), Y, XV, YV, A, Z) :-                                   
    (
        V=XV -> prependAllC(K, Y, YV, A, LY);
        LY=A
    ),
    cartesianProduct(L, Y, XV, YV, LY, Z0),                                             
    cartesianProduct(R, Y, XV, YV, Z0, Z).                                              

*/

% Create pairs of given element with all nodes of given BST.
prependAll(X, Y, Z) :- prependAll(X, Y, [], Z).
prependAll(_, empty, A, A).
prependAll(X, node(L, K-_, R), A, Z) :-
    prependAll(X, L, [(X-K) | A], Z0),
    prependAll(X, R, Z0, Z).

/*
 % Similar to above, only with a value constraint.
prependAllC(X, Y, YV, Z) :- prependAllC(X, Y, YV, [], Z).
prependAllC(_, empty, _, A, A).
prependAllC(X, node(L, K-V, R), YV, A, Z) :-
    (
        V=YV -> prependAllC(X, L, YV, [(X-K) | A], Z0);
        prependAllC(X, L, YV, A, Z0)
    ),
    prependAllC(X, R, YV, Z0, Z).
*/

% checkFullGraph(States, Alphabet, Transitions) true iff single outgoing
% edge for each letter in alphabet.
checkFullGraph(S, A, T) :-
    cartesianProduct(S, A, P),
    allPresent(P, T).

% Insert all final states with corresponding value.
insertAllBST([], T, T).
insertAllBST([L | R], T, Z) :-
    insertBST(T, L-1, T0),
    insertAllBST(R, T0, Z).

% dfaInternal(states, alphabet, transitions, start, final)

% correct(+Automat, -Reprezentacja)
correct(dfa(TF, SS, FS), dfaInternal(S1, T, AL)) :-
    getTransitions(TF, T),
    getStates(TF, S),
    getAlphabet(TF, A),
    checkFullGraph(S, A, T),
    allPresent(FS, S),
    getKeyBST(S, SS, _),
    toListBST(A, AL),
    insertAllBST(FS, S, S1).

accept(dfa(TF, SS, FS), L) :-
    correct(dfa(TF, SS, FS), dfaInternal(S, T, A)),
    acceptHelper(SS, S, T, A, L).

acceptHelper(SS, S, _, _, []) :-
    getKeyBST(S, SS, 1).

acceptHelper(SS, S, T, A, [L | R]) :-
    getKeyBST(T, SS-L, N),
    acceptHelper(N, S, T, A, R).

% empty(+Automat)
empty(dfa(TF, SS, FS)) :- \+finalPath(dfa(TF, SS, FS)).

finalPath(dfa(TF, SS, FS)) :-
    correct(dfa(TF, SS, FS), dfaInternal(S, T, A)),
    findFinalPath(SS, S, T, A, []).

findFinalPath(C, S, _, _, _) :-
    getKeyBST(S, C, 1),
    !.

findFinalPath(C, S, T, A, V) :-
    member(NX, A),
    getKeyBST(T, C-NX, NS),
    \+ member(NS, V),
    findFinalPath(NS, S, T, A, [NS | V]).

% equal(+Automat1, +Automat2)
equal(dfa(TF_A, SS_A, FS_A), dfa(TF_B, SS_B, FS_B)) :-
    correct(dfa(TF_A, SS_A, FS_A), dfaInternal(ST_A, T_A, A_A)),
    correct(dfa(TF_B, SS_B, FS_B), dfaInternal(ST_B, T_B, A_B)),
    equalList(A_A, A_B),
    construct(T_A, T_B, A_A, [fp(0,1,(SS_A-SS_B))], TM, []),
    
    %cartesianProduct(ST_A, ST_B, 1, 0, FA),
    %cartesianProduct(ST_A, ST_B, 0, 1, FB),

    getStates(TM, ST),
    
    toListBST(ST, STL),
    unionConstraint(STL, ST_A, ST_B, 1, 0, FA),
    unionConstraint(STL, ST_A, ST_B, 0, 1, FB),
    append(FA, FB, FMERGE),
    empty(dfa(TM, SS_A-SS_B, FMERGE)).

% Correct iff list contains final states of the product construction.
unionConstraint(L, F, S, VF, VS, M) :- unionConstraint(L, F, S, VF, VS, [], M).
unionConstraint([], _, _, _, _, M, M).
unionConstraint([(A-B) | R], F, S, VF, VS, AC, M) :-
    getKeyBST(F, A, V1),
    getKeyBST(S, B, V2),
    (
        V1=VF, V2=VS ->
        unionConstraint(R, F, S, VF, VS, [A-B | AC], M);
        unionConstraint(R, F, S, VF, VS, AC, M)
    ).

equalList(L1, L2) :- 
    \+ (member(X, L1), \+ member(X, L2)),
    \+ (member(X, L2), \+ member(X, L1)).

% Given starting nodes, transitions and alphabet merge two DFAs by
% using product construction.
getAll(_, _, _, _, [], []).
getAll(SA, SB, TA, TB, [X | A], [fp((SA-SB), X, (NA-NB)) | L]) :-
     getKeyBST(TA, SA-X, NA),
     getKeyBST(TB, SB-X, NB),
     getAll(SA, SB, TA, TB, A, L).

construct(TA, TB, A, SA, TM, V) :- 
    productConstruction(TA, TB, A, SA, TM, [], V).

productConstruction(_, _, _, [], TM, TM, _).

productConstruction(TA,
                    TB,
                    A,
                    [fp(_,_,(SA-SB)) | K],
                    TM,
                    TMA,
                    V) :-
     member(SA-SB, V),
     !,
     productConstruction(TA, TB, A, K, TM, TMA, V).

productConstruction(TA, 
                    TB, 
                    A,
                    [fp(_,_,(SA-SB)) | K],
                    TM,
                    TMA,
                    V) :-
    getAll(SA, SB, TA, TB, A, N),
    append(K, N, K1),
    append(N, TMA, TMAN),
    productConstruction(TA, TB, A, K1, TM, TMAN, [SA-SB | V]).

%subsetEq(+Automat1, +Automat2)
subsetEq(dfa(TF_A, SS_A, FS_A), dfa(TF_B, SS_B, FS_B)) :-                          
     correct(dfa(TF_A, SS_A, FS_A), dfaInternal(ST_A, T_A, A_A)),                
     correct(dfa(TF_B, SS_B, FS_B), dfaInternal(ST_B, T_B, A_B)),                
     equalList(A_A, A_B),                                                        
     construct(T_A, T_B, A_A, [fp(0,1,(SS_A-SS_B))], TM, []),                    
     %cartesianProduct(ST_A, ST_B, 1, 0, FA),                                     
      
     getStates(TM, ST),

     toListBST(ST, STL),
     unionConstraint(STL, ST_A, ST_B, 1, 0, FA),

     empty(dfa(TM, SS_A-SS_B, FA)).

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
