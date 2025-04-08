man(john).
woman(mary).

loves(john, mary).
loves(john, beer).
loves(mary, beer).
loves(john, X) :- loves(X, beer).

% o, s(o), s(s(o)), ...

nat(o).
nat(s(X)) :- nat(X).

% add(?X, ?Y, ?Z) ⟷ X + Y = Z
add(o, Y, Y).
add(s(X), Y, s(Z)) :- add(X, Y, Z).

% mem(?X, ?L)
mem(X, [X | _]).
mem(X, [_ | T]) :- mem(X, T).
