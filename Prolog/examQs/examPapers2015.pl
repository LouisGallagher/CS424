

noah([], [], []).
noah([X | Xs], [Y | Ys], [X,Y | Zs]) :- noah(Xs, Ys, Zs).