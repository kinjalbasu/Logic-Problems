
:- use_module(library(clpfd)).
zebra(Vars):-
	Vars=[N1,N2,N3,N4,N5,
			C1,C2,C3,C4,C5,
			P1,P2,P3,P4,P5,
			A1,A2,A3,A4,A5,
			D1,D2,D3,D4,D5],
	% There are five houses.
	Vars ins 1..5,
	% The Englishman lives in the red house.*/
	
	N1 #= C2,
	% The Spain owns the dog.
	N2 #= A1,	
	% Japaneese is a painter.
	N3 #= P1,
	
	%The Italian Drink Tea
	N4 #= D3,
	
	%The Norweign Lives in the first house on the left
	N5 #= 1,
	
	%The owner of the green house drinks cofee
	C1 #= D4,
	
	%The green house is on the right of the white one
	C1 #= C5 + 1,
	
	%The sculptors breed snail
	P5 #= A4,
	
	%The Diplomat lives in the yellow house
	P2 #= C3,
	
	%Milk is drunk in the middle house.
	D5 #= 3,
	%The Norweign house is next to the blue one.
	N5 #= C4-1#\/N5 #= C4+1,
	
	%The violinist drinks fruit juice
	P3 #= D1,
	
	%The fox is in the house next to that of the doctor.
	A3 #= P4-1#\/A3 #= P4+1,
	
	%The horse is in the house next to that of the diplomat.
	A5 #= P2-1#\/A5 #= P2+1,
	
	all_different([N1,N2,N3,N4,N5]),
	all_different([C1,C2,C3,C4,C5]),
	all_different([P1,P2,P3,P4,P5]),
	all_different([A1,A2,A3,A4,A5]),
	all_different([D1,D2,D3,D4,D5]),
	label(Vars).
