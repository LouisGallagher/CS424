

noah([], [], []).
noah([X | Xs], [Y | Ys], [X,Y | Zs]) :- noah(Xs, Ys, Zs).

member(X,[X | _ ]).
member(X, [_| Ys]) :- member(X, Ys).

doubleMember(X, [ Y | Ys]) :- X==Y, member(X, Ys); doubleMember(X, Ys). 


sibling([[Z,X],[Z,Y]],X,Y) :- X \== Y.
sibling([[Z,Y],[Z,X]],X,Y) :- X \== Y.
sibling([Z, X], X, Y).
sibling([Z, Y], X, Y).
sibling([], X, Y):-.