%Crypto(Word1,Word2,Word3) : Prints the assigned digits else "No"
crypto_arith(Word1,Word2,Word3) :- solution(Word1, Word2, Word3);(write("no"),fail).

%solution(W1,W2,W3) : recursively checks and print
solution(W1,W2,W3) :-
    append(W1,W2,TempList),
    append(TempList,W3,AllWord),
    value_assignment([0,1,2,3,4,5,6,7,8,9],AllWord),
    add_zero(W1,WithZero1),
    add_zero(W2,WithZero2),
    add_zero(W3,WithZero3),
    name(Num1,WithZero1),
    name(Num2,WithZero2),
    name(SumNum,WithZero3),
    SumNum is Num1+Num2,
    !.
	
remove(X,[X|Xs],Xs).

remove(X,[Y|Ys],[Y|Res]):-
    remove(X,Ys,Res).
value_assignment(Digits,[X|Tail]) :-
    nonvar(X),
    !,
    value_assignment(Digits,Tail).
value_assignment(Digits,[X|Tail]) :-
    remove(X,Digits,D1),
    value_assignment(D1,Tail).
value_assignment(_,[]) :-
    !.

add_zero([],[]) :-
    !.
add_zero([X|Tail1],[Y|Tail2]) :-
    !,
    Y is X+48,
    add_zero(Tail1,Tail2).
	