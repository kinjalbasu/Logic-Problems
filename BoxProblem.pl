%Box Problem
transform(State1,State2,Plan) :- transform(State1,State2,[State1],Plan).
transform(State,State,_,[ ]).
transform(State1,State2,Visited,[Action|Actions]) :- choose_action(Action,State1,State2),update(Action,State1,State), \+(member(State,Visited)),transform(State,State2,[State|Visited],Actions).

%define actions
choose_action(Action,State1,State2):- suggest(Action,State2), legal_action(Action,State1).
choose_action(Action,State1,_) :- legal_action(Action,State1).

%Optial soltuion
suggest(to_place(X,_,Z),State) :- member(on(X,Z),State), place(Z).
suggest(to_block(X,_,Z),State) :- member(on(X,Z),State), block(Z).

%checks legal moves
legal_action(to_place(Block,Y,Place),State) :- on(Block,Y,State), clear(Block,State), place(Place), clear(Place,State).
legal_action(to_block(Block1,Y,Block2),State) :- on(Block1,Y,State), clear(Block1,State), block(Block2),Block1 \= Block2, clear(Block2,State).

clear(X,State) :- \+(member(on(_,X) ,State)).
on(X,Y,State) :- member(on(X,Y),State).

%update using substitute with the new data
update(to_block(X,Y,Z),State,State1) :-substitute1(on(X,Y),on(X,Z),State,State1).
update(to_place(X, Y, Z), State, State1) :- substitute1(on(X,Y),on(X,Z),State,State1).


place(p).
place(q).
place(r).
block(a).
block(b).
block(c).
block(d).
block(e).

%substitute element
substitute1(_,_,[],[]).
substitute1(X,Y,[X|T],[Y|Z]) :- substitute1(X,Y,T,Z).
substitute1(X,Y,[H1|T],[H1|Z]) :- substitute1(X,Y,T,Z).