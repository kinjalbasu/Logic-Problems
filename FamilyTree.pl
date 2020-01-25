father(tony,abe).
father(tony,sarah).
father(abe,john).
father(john,jill).
father(bill,susan).
father(rob,jack).
father(rob,phil).
father(jack,jim).
mother(lisa,abe).
mother(lisa,sarah).
mother(nancy,john).
mother(mary,jill).
mother(sarah,susan).
mother(susan,jack).
mother(susan,phil).
male(tony).
male(abe).
male(bill).
male(john).
male(rob).
male(jack).
male(phil).
male(jim).
male(rick).
female(lisa).
female(sarah).
female(susan).
female(nanacy).
female(mary).
female(jill).
female(kim).
female(martha).
female(ann).

married(lisa,tony).
married(nancy,abe).
married(mary,john).
married(jill,rick).
married(sarah,bill).
married(susan,rob).
married(kim,jack).
married(ann,phil).
married(martha,jim).

parent(X,Y) :- father(X,Y).
parent(X,Y) :- mother(X,Y).
sibling(X,Y) :- parent(Z,X), parent(Z,Y), not(X=Y).
fcousin(X,Y) :- parent(M,X),parent(N,Y),sibling(M,N).

scousin(X,Y) :- parent(M,X),parent(N,Y),fcousin(M,N).

grnephew(X,Y) :- sibling(Y,Z),parent(Z,M),parent(M,X),male(X).

niece(X,Y) :- female(X),parent(Z,X),sibling(Z,Y).

manc(X,Y) :- male(X),parent(X,Y).
manc(X,Y) :- parent(Z,Y),manc(X,Z).

sibgen(X,Y) :- parent(M,X),parent(N,Y),sibling(M,N).
sibgen(X,Y) :- parent(M,X),parent(N,Y),sibgen(M,N).

niece(X,Y) :- female(X), married(X,Z), parent(M,Z), sibling(M,Y).
grnephew(X,Y) :- male(X), married(Z,X), parent(M,Z),parent(N,M),sibling(N,Y).