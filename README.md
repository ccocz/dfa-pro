# DFA

## Main predicates

### accept

```prolog
?- accept(+Automata, ?Word)
```

iff word is accepted by `Automata`. If `Word` is a variable, predicate
generates all words accepted by `Automata`.

### equal

```prolog
?-  (+Automata1, +Automata2)
```

iff L(Automata1) = L(Automata2).

### subsetEq

```prolog
?- subsetEq(+Automata1, +Automata2)
```

iff L(Automata1) ⊆ L(Automata2).

## Tests
After loading both `dfa.pl` and `unit_tests.pl`, run

```prolog
?- run_tests.
```
