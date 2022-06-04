% @author Resul Hangeldiyev

/*
 * todos:
 * - check if x-y pair representation is valid
 */

% memberchk, punktu nadbiour
% visited as bst

:- use_module(library(lists)).

% insertBST(+T, +(A-B), -R)
insertBST(node(L, K-V, R), X-Y, node(L1, K-V, R)) :-
    X @< K,
    insertBST(L, X-Y, L1).
insertBST(node(L, K-V, R), X-Y, node(L, K-V, R1)) :-
    X @> K,
    insertBST(R, X-Y, R1).

insertBST(node(L, K-_, R), K-V0, node(L, K-V0, R)).
insertBST(empty, K-V, node(empty, K-V, empty)).

% initPair(+(X-Y)) iff non of the tuple elements contain uninitialized value.
initPair(X-Y) :-
    \+ var(X),
    \+ var(Y),
    initPair(X), initPair(Y).

% memberBST(+T, +K, ?V) iff K is present in the given 
% BST and has V value attached.
memberBST(node(_, K-V, _), K, V).

memberBST(node(L, K-_, _), K0, V) :-
    initPair(K0),
    K0 @< K,
    !,
    memberBST(L, K0, V).

memberBST(node(L, K-_, _), K0, V) :-
    initPair(K0),
    K0 @> K,
    !,
    memberBST(L, K0, V).

memberBST(node(_, _, R), K, V) :-
    \+ initPair(K),
    memberBST(R, K, V).

memberBST(node(L, _, _), K, V) :-
    \+ initPair(K),
    memberBST(L, K, V).

% allPresent(+List, +BST) iff all keys from list appears in BST.
allPresent([], _).
allPresent([L | R], T) :-
    memberBST(T, L, _),
    allPresent(R, T).

% toListBST(+T, -L) iff L is equal to set of all keys contained in T.
toListBST(T, L) :- toListBST(T, [], L).
toListBST(empty, A, A).
toListBST(node(L, K0-_, R), A, K) :-
  toListBST(R, A, K1),
  toListBST(L, [K0 | K1], K).

% dfa(TransitionFunction, StartingState, FinalStates)

transitionFunction([]).
transitionFunction([fp(_, _, _) | R]) :- transitionFunction(R).

dfa(TF, _, _) :- transitionFunction(TF).

% states(+L, -T) iff T is equal to BST of states constructed from 
% transitions L. Initially all states have 0 value assigned, 
% 1 represents final states.
states(L, T) :- states(L, empty, T).
states([], T, T).
states([fp(S1, _, S2) | Y], A, T) :-
    insertBST(A, S1-0, A0),
    insertBST(A0, S2-0, A1),
    states(Y, A1, T).

% alphabet(+L, -T) iff T is equal to BST of letters constructed 
% from transitions L.
alphabet(L, T) :- alphabet(L, empty, T).
alphabet([], T, T).
alphabet([fp(_, C, _) | Y], A, T) :-
    insertBST(A, C-C, A0),
    alphabet(Y, A0, T).

% transitions(+L, -T) iff T contains all edges from L such 
% that edge (S1, C, S2) S2 is mapped as value of the key (S1-C).
transitions(L, T) :- transitions(L, empty, T).
transitions([], T, T).
transitions([fp(S1, C, S2) | Y], A, T) :-
    \+ memberBST(A, S1-C, _),
    insertBST(A, (S1-C)-S2, A0),
    transitions(Y, A0, T).

% doAllExist(+L, +T) iff all keys in L exist in T.
doAllExist([], _).
doAllExist([X | Y], T) :-    
    memberBST(T, X, _),
    doAllExist(Y, T).

% cartesianProduct(+BST_X, +BST_Y, List) 
% iff List = [keys of BST_X] * [keys of BST_Y].
cartesianProduct(X, Y, Z) :- cartesianProduct(X, Y, [], Z).
cartesianProduct(empty, _, A, A).
cartesianProduct(_, empty, A, A).
cartesianProduct(node(L, K-_, R), Y, A, Z) :-
    prepended(K, Y, A, LY),
    cartesianProduct(L, Y, LY, Z0),
    cartesianProduct(R, Y, Z0, Z).

% prepended(+X, +T, -Z) iff Z is set of (X-Z0) pairs where Z0 is member of Z.
prepended(X, Y, Z) :- prepended(X, Y, [], Z).
prepended(_, empty, A, A).
prepended(X, node(L, K-_, R), A, Z) :-
    prepended(X, L, [(X-K) | A], Z0),
    prepended(X, R, Z0, Z).

% entireFunction(+S, +A, +T) iff T is entire transition function.
entireFunction(S, A, T) :-
    cartesianProduct(S, A, P),
    allPresent(P, T).

% insertAllBST(+L, +T0, -T1).
insertAllBST([], T, T).
insertAllBST([L | R], T, Z) :-
    insertBST(T, L-1, T0),
    insertAllBST(R, T0, Z).

% dfaInternal(states, transitions, alphabet)

% correct(+Automata, -Representation)
correct(dfa(TF, SS, FS), dfaInternal(S1, T, AL, DET)) :-
    transitions(TF, T),
    states(TF, S),
    alphabet(TF, A),
    entireFunction(S, A, T),
    allPresent(FS, S),
    memberBST(S, SS, _),
    toListBST(A, AL),
    insertAllBST(FS, S, S1),
    aliveNodes(T, S1, S1, AL, DE),
    insertAllBST(DE, empty, DET).

/* aliveNodes(+Transitions, 
             +CurrentState
             +States
             +Alphabet
             +AliveNodes)
             
             iff AlivesNodes is list of nodes from which exists
             a path to a final node.
             */
aliveNodes(T, S, CS, A, AL) :- aliveNodes(T, S, CS, A, [], AL).
aliveNodes(_, empty, _, _, AL, AL).
aliveNodes(T, node(L, ST-_, R), CS, A, AC, AL) :-
    aliveNodes(T, R, CS, A, AC, AL1),
    (
        findFinalPath(ST, CS, T, A, []) ->
        aliveNodes(T, L, CS, A, [ST | AL1], AL);
        aliveNodes(T, L, CS, A, AL1, AL)
    ).

% accept(+Automata, ?Word)
accept(dfa(TF, SS, FS), L) :-
    correct(dfa(TF, SS, FS), dfaInternal(S, T, A, DE)),
    acceptHelper(SS, S, T, A, L, DE).

acceptHelper(SS, S, _, _, [], _) :-
    memberBST(S, SS, 1).

acceptHelper(SS, S, T, A, [L | R], DE) :-
    (
        memberBST(DE, SS, _) ->
        memberBST(T, SS-L, N),
        acceptHelper(N, S, T, A, R, DE);
        !, fail                             % looped into cycle, failing
    ).

% empty(+Automata)
empty(dfa(TF, SS, FS)) :- \+ finalPath(dfa(TF, SS, FS)).

% finalPath(+Automata) iff there exists a path 
% from starting state to a final state 
finalPath(dfa(TF, SS, FS)) :-
    correct(dfa(TF, SS, FS), dfaInternal(_, _, _, DE)),
    memberBST(DE, SS, _).

findFinalPath(C, S, _, _, _) :-
    memberBST(S, C, 1),
    !.

findFinalPath(C, S, T, A, V) :-
    member(NX, A),
    memberBST(T, C-NX, NS),
    \+ member(NS, V),
    findFinalPath(NS, S, T, A, [NS | V]).

% equal(+Automata1, +Automata2)
equal(dfa(TF_A, SS_A, FS_A), dfa(TF_B, SS_B, FS_B)) :-
    correct(dfa(TF_A, SS_A, FS_A), dfaInternal(ST_A, T_A, A_A, _)),
    correct(dfa(TF_B, SS_B, FS_B), dfaInternal(ST_B, T_B, A_B, _)),
    equalList(A_A, A_B),
    construct(T_A, T_B, A_A, [fp(0,1,(SS_A-SS_B))], TM, []),
    states(TM, ST),
    toListBST(ST, STL),
    unionConstraint(STL, ST_A, ST_B, 1, 0, FA),
    unionConstraint(STL, ST_A, ST_B, 0, 1, FB),
    append(FA, FB, FMERGE),
    empty(dfa(TM, SS_A-SS_B, FMERGE)).

% unionConstraint(+L, +F, +S, +VF, +VS, -M) iff L contains final 
% states of the product construction.
unionConstraint(L, F, S, VF, VS, M) :- 
    unionConstraint(L, F, S, VF, VS, [], M).
unionConstraint([], _, _, _, _, M, M).
unionConstraint([(A-B) | R], F, S, VF, VS, AC, M) :-
    memberBST(F, A, V1),
    memberBST(S, B, V2),
    (
        V1=VF, V2=VS ->
        unionConstraint(R, F, S, VF, VS, [A-B | AC], M);
        unionConstraint(R, F, S, VF, VS, AC, M)
    ).

equalList(L1, L2) :- 
    \+ (member(X, L1), \+ member(X, L2)),
    \+ (member(X, L2), \+ member(X, L1)).

/* allProducts(+stateA,
              +stateB,
              +transitionsA,
              +transitionsB,
              +alphabet,
              -result)
   iff result is list of edges which is constructed following 
   product construction. */
allProducts(_, _, _, _, [], []).
allProducts(SA, SB, TA, TB, [X | A], [fp((SA-SB), X, (NA-NB)) | L]) :-
     memberBST(TA, SA-X, NA),
     memberBST(TB, SB-X, NB),
     allProducts(SA, SB, TA, TB, A, L).

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
    allProducts(SA, SB, TA, TB, A, N),
    append(K, N, K1),
    append(N, TMA, TMAN),
    productConstruction(TA, TB, A, K1, TM, TMAN, [SA-SB | V]).

%subsetEq(+Automata1, +Automata2)
subsetEq(dfa(TF_A, SS_A, FS_A), dfa(TF_B, SS_B, FS_B)) :-                          
     correct(dfa(TF_A, SS_A, FS_A), dfaInternal(ST_A, T_A, A_A, _)),                
     correct(dfa(TF_B, SS_B, FS_B), dfaInternal(ST_B, T_B, A_B, _)),                
     equalList(A_A, A_B),                                                        
     construct(T_A, T_B, A_A, [fp(0,1,(SS_A-SS_B))], TM, []),
     states(TM, ST),
     toListBST(ST, STL),
     unionConstraint(STL, ST_A, ST_B, 1, 0, FA),
     empty(dfa(TM, SS_A-SS_B, FA)).
