%Insertion Sort Algorithm
insertionsort([], []).
insertionsort([H | Xs], Ys) :- insertionsort(Xs, Zs), insert(H, Zs, Ys).

insert(X, [], [X]).
insert(X, [Y | Ys], [Y | Zs]) :- X > Y, insert(X, Ys, Zs).
insert(X, [Y | Ys], [X, Y | Ys]) :- X =< Y.

%Remove last element of a list
remove_last([], []) :- !.
remove_last([_], []) :- !.
remove_last([X | T], [X | T2]) :- remove_last(T, T2).

%Median of a list
middle([X], X).
middle([_,X], X).
middle([H|T], X) :-
    remove_last(T, TWithoutLast),
    middle(TWithoutLast, X).
	
%medians of median
list_of_median([],[]).
list_of_median([A,B,C,D,E|T],[Z|Zs]) :- insertionsort([A,B,C,D,E],Y), middle(Y,Z), list_of_median(T,Zs).
list_of_median(X,[Z]) :- length(X,P1), P1<5, insertionsort(X,Y),middle(Y,Z).

get_one_median([X|[]],X) :-!.
get_one_median([H|T],Z) :- list_of_median([H|T],Y), get_one_median(Y,Z).


%Kth Largest Element Algo
kth_largest(Xs,K,Z) :- get_one_median(Xs,M), partition(Xs,M,Left,Equal,Right), length(Right,C1), length(Equal,C2), C is C1+C2, C1<K, C >= K, Z = M.
kth_largest(Xs,K,Z) :- get_one_median(Xs,M), partition(Xs,M,Left,Equal,Right), length(Right,C1), C1 >= K, kth_largest(Right,K,Z).
kth_largest(Xs,K,Z) :- get_one_median(Xs,M), partition(Xs,M,Left,Equal,Right), length(Right,C1), length(Equal,C2), C is C1+C2, C < K, K1 is K-C,kth_largest(Left,K1,Z).

%Partition Algorithm with Equal Values
partition([],_,[],[],[]).
partition([H|T],X,Left,[H|Equal],Right) :- H=X , partition(T,X,Left,Equal,Right).
partition([H|T],X,[H|Left],Equal,Right) :- H<X , partition(T,X,Left,Equal,Right).
partition([H|T],X,Left,Equal,[H|Right]) :- H>X , partition(T,X,Left,Equal,Right).