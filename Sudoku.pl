
:- use_module(library(clpfd)).

sudoku1(Input) :-
				input_to_list(Input,ListofTerms),
				twoD_list(M),
				insert_in_twoDList(M,ListofTerms,M1),
				sudoku(M1).


				
%Main Logic
sudoku(Rows) :-
        length(Rows, 9), maplist(same_length(Rows), Rows),
        append(Rows, Vs), Vs ins 1..9,
        maplist(all_distinct, Rows),
        transpose(Rows, Columns),
        maplist(all_distinct, Columns),
        Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
        blocks(As, Bs, Cs),
        blocks(Ds, Es, Fs),
        blocks(Gs, Hs, Is),
		write_ListofList(Rows).

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
        all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
        blocks(Ns1, Ns2, Ns3).				
				
%Input to List
input_to_list(Input,Ops) :-
	X = [],
    open(Input, read, Str),
    read_file(Str,Lines),
    close(Str),
    append(X,Lines,Ops).

read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    read_file(Stream,L).


% Insert Each List Element to 2D-List.
insert_in_twoDList(M,[],M).
insert_in_twoDList(M,[Term|T],P) :- Term =.. [_|Args], insert_at(Args,M,M1), insert_in_twoDList(M1,T,P).

%Get Term - f(X,Y,Z) to Variables - X,Y,Z


% Insert X,Y,Z into the 2D-List
insert_at([X,Y,Z|[]],M,P) :- insert_at_ListOfList(X,Y,Z,M,P).	  
	  
insert_at_ListOfList(1,Y,Z,[List_Head|T1],[NewList_Head|T1]) :- insert_at_List(Y,Z,List_Head,NewList_Head).	  
insert_at_ListOfList(X,Y,Z,[List_Head|T1],[List_Head|T2]) :-
	X > 1, X1 is X - 1, insert_at_ListOfList(X1,Y,Z,T1,T2). 	

insert_at_List(1,Z,[_|T1],[Z|T1]).
insert_at_List(Y,Z,[H1|T1],[H1|T2]) :- Y > 1, Y1 is Y - 1,insert_at_List(Y1,Z,T1,T2).


%Write the Solved Puzzle
write_ListofList([]).
write_ListofList([H|ListOfList]) :- write(H),nl,write_ListofList(ListOfList).



%2D-List
twoD_list([[_,_,_,_,_,_,_,_,_],
	[_,_,_,_,_,_,_,_,_],
	[_,_,_,_,_,_,_,_,_],
	[_,_,_,_,_,_,_,_,_],
	[_,_,_,_,_,_,_,_,_],
	[_,_,_,_,_,_,_,_,_],
	[_,_,_,_,_,_,_,_,_],
	[_,_,_,_,_,_,_,_,_],
	[_,_,_,_,_,_,_,_,_]]).
	
