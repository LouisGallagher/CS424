

noah([], [], []).
noah([X | Xs], [Y | Ys], [X,Y | Zs]) :- noah(Xs, Ys, Zs).

member(X,[X | _ ]).
member(X, [Y | Ys]) :- member(X, Ys).

doubleMember(X, [ Y | Ys]) :- X==Y, member(X, Ys); doubleMember(X, Ys). 
