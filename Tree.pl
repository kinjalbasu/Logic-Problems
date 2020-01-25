treemember(X,node(X,_,_)).
treemember(X,node(Y,L,R)) :- X<Y, treemember(X,L).
treemember(X,node(Y,L,R)) :- X>Y, treemember(X,R).

successor(0,0).
successor(N,s(Z)) :- N>0, N1 is N-1, successor(N1,Z).

plus(0,Y,Y).
plus(s(X),Y,s(Z)) :- plus(X,Y,Z).

sumtree(nil, 0).
sumtree(node(X,L,R), Z) :- sumtree(L, Z1), sumtree(R, Z2), successor(X,X1),plus(X1,Z1,Y),plus(Y,Z2,Z).



delete(X, node(T, L, G), node(T, Aux, G)) :- X<T, !, delete(X, L, Aux).
delete(X, node(T, L, G), node(T, L, Aux)) :- X>T, !, delete(X, G, Aux).
 
delete(X, node(X, L, nil), L) :- !.
delete(X, node(X, nil, G), G) :- !.
delete(X, node(X, L, G), node(M, L, Aux)) :- delete_min(G, Aux, M).
 
delete_min(node(T, nil, R), R, T) :- !.
delete_min(node(T, L, G), node(T, Aux, G), M) :- delete_min(L, Aux, M).