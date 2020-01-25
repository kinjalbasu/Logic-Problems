plus(0,Y,Y).
plus(succ(X),Y,succ(Z)) :- plus(X,Y,Z).

times(succ(0),Y,Y).
times(0,Y,0).
times(succ(X),Y,A) :- times(X,Y,Z),plus(Z,Y,A).

greatt(succ(X),0).
greatt(succ(X),succ(Y)) :- greatt(X,Y).


fact(0,succ(0)).
fact(succ(X),A) :- fact(X,B),times(succ(X),B,A).


fib(1,0).
fib(2,succ(0)).
fib(N,F) :- N1 is N-1, N2 is N-2,fib(N2,B),fib(N1,A),plus(A,B,F).

equal(0,0).
equal(succ(X),succ(Y)) :- equal(X,Y).


div(0,_,_,_):- false.
div(succ(X),succ(Y),Q,R) :- times(succ(X),Q,A),plus(A,R,B),equal(B,succ(Y)),greatt(succ(X),R).

 
