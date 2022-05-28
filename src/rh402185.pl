% @author Resul Hangeldiyev


% dfa(TransitionFunction, StartingState, FinalStates)

transitionFunction([]).
transitionFunction([fp(_, _, _) | R]) :- transitionFunction(R).

dfa(TF, _, _):- transitionFunction(TF).

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
