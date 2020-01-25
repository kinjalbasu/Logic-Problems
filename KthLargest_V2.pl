partition([], _, [], [], []).
partition([H | Ls], X, Smalls, [H | Equals], Bigs) :- H == X, partition(Ls, X, Smalls, Equals, Bigs).
partition([H | Ls], X, [H | Smalls], Equals, Bigs) :- H < X, partition(Ls, X, Smalls, Equals, Bigs).
partition([H | Ls], X, Smalls, Equals, [H | Bigs]) :- H > X, partition(Ls, X, Smalls, Equals, Bigs).

% insert(X, Xs, Ys) :- insert X at the proper place in an ordered list Xs so that it results in an ordered list Ys.
insert(X, [], [X]).
insert(X, [Y | Ys], [Y | Zs]) :- X > Y, insert(X, Ys, Zs).
insert(X, [Y | Ys], [X, Y | Ys]) :- X =< Y.

% insertionsort(Xs, Ys) :- Ys is the sorted permutation of Xs.
insertionsort([], []).
insertionsort([H | Xs], Ys) :- insertionsort(Xs, Zs), insert(H, Zs, Ys).

% medians(Xs, Ms) :- Ms is the list of medians of groups when Xs is broken into groups of 5.
medians([], []).
medians([M], [M]).
medians([X1, X2], [M]) :- insertionsort([X1, X2], [M, _]).
medians([X1, X2, X3], [M]) :- insertionsort([X1, X2, X3], [_, M, _]).
medians([X1, X2, X3, X4], [M]) :- insertionsort([X1, X2, X3, X4], [_, M, _, _]).
medians([X1, X2, X3, X4, X5 | Xs], [M | Ms]) :- insertionsort([X1, X2, X3, X4, X5], [_, _, M, _, _]), medians(Xs, Ms).

% median_of_medians(Xs, M) :- M is the recursive median of medians of Xs.
median_of_medians([M], M).
median_of_medians([H1, H2 | Xs], M) :- medians([H1, H2 | Xs], Ms), median_of_medians(Ms, M).

% kth_largest(Xs, K, E) :- E is the Kth largest element of the list Xs.
kth_largest([E], 1, E).
kth_largest(Xs, K, M) :- median_of_medians(Xs, M), partition(Xs, M, Smalls, Equals, Bigs), length(Equals, LenEq), length(Bigs, LenBigs), LenGE is LenEq + LenBigs, K > LenBigs, LenGE >= K.
kth_largest(Xs, K, E) :- median_of_medians(Xs, M), partition(Xs, M, Smalls, Equals, Bigs), length(Bigs, LenBigs), LenBigs >= K, kth_largest(Bigs, K, E).
kth_largest(Xs, K, E) :- median_of_medians(Xs, M), partition(Xs, M, Smalls, Equals, Bigs), length(Equals, LenEq), length(Bigs, LenBigs), LenGE is LenEq + LenBigs, K > LenGE, NewK is K - LenGE, kth_largest(Smalls, NewK, E).

