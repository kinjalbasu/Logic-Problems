% N - Queens 
%Permutation(Xs,Zs): Zs are the all permutation for Xs
permutation(Xs,[Z|Zs]) :- select(Z,Xs,Ys),permutation(Ys,Zs).
permutation([],[]).

%range(M,N,Ns): Return List of integer in between M and N
range(M,N,[M|Ns]) :- M<N , M1 is M+1, range(M1,N,Ns).
range(N,N,[N]).

%queens(N,Qs): N is the col size. Qs will be returned as queens position in each col
queens(N,Qs) :- range(1,N,Ns), permutation(Ns,Qs), safe(Qs).

%Checks if Q is safe or not in Qs
safe([Q|Qs]) :- safe(Qs), not(attack(Q,Qs)).
safe([]).

%Check if X is attacked by any of the Xs.
attack(X,Xs) :- attack(X,1,Xs).
attack(X,N,[Y|_]) :- X is Y+N ; X is Y-N.
attack(X,N,[_|Ys]) :- N1 is N+1, attack(X,N1,Ys).