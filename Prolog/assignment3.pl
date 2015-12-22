
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Part 1 interleave %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% get the elements of a list at even indices
evens([], []).
evens([X, _ | L],[X | R]) :- evens(L, R).

%% get the elements of a list at odd indices
odds([],[]).
odds([_, X|L], [X | R]) :- odds(L, R).

%% are two variables equal?
equal(X,X).

%% are Zs the interleave of Xs and Ys? they are if the elements at the even indices of Zs form a set equal to Xs
%% and the elements at odd indices of Zs form a set equal to Ys.
interleave(Xs, Ys, Zs) :- evens(Zs, E), odds(Zs, O), equal(Xs, E) , equal(Ys, O).



%% for debug purposes 
print_odds(L, R) :- odds(L,R), print(R).
print_evens(L, R) :- evens(L,R), print(R) .


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Part 2 Cousins %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ancestry Database
parent(patrick, louis).
parent(patrick, frank).
parent(michael, patrick).

parent(frances, harry).
parent(michael, frances).

grandparent(X,Y):- parent(X,Z), parent(Z,Y).
sibling(X,Y) :- parent(Z, X), parent(Z, Y).


%% 1st cousins share a grandparent. This relation will claim that siblings are cousins when they share a grandparent( which they invariably will
%% being siblings and all). The relation will claim that a person is there own cousin since they share a grandparent with themselves. 
%% This behaviour can be avoided by introducing checks to ensure that:
%% 1) X != Y.
%% 2) sibling(X,Y) == false.
%% although one could argue that the only problem is the semantics of the word 'cousin'.

cousin(X,Y) :- grandparent(Z,X), grandparent(Z,Y),  \+ equal(X,Y) , \+ sibling(X,Y).

%% Aside:  1st cousin is a child of ones uncle or aunt. This offers and alternate formulation of the relation cousin\2
%% cousin(X, Y) :- parent(Z, X), parent(W, Y), sibling(Z,W).