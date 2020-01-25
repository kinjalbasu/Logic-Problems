% Cannibal Missionary

getpath :- transport([3,3,0,0,left],[0,0,3,3,right],[[3,3,0,0,left]],_).


% Main Transport logic
transport([Mis_Left1,Can_Left1,Mis_Right1,Can_Right1,Boat_Pos1],[Mis_Left2,Can_Left2,Mis_Right2,Can_Right2,Boat_Pos2],Explored_States,Moves) :- 
   move([Mis_Left1,Can_Left1,Mis_Right1,Can_Right1,Boat_Pos1],[Mis_Left3,Can_Left3,Mis_Right3,Can_Right3,Boat_Pos3]), 
   not(member([Mis_Left3,Can_Left3,Mis_Right3,Can_Right3,Boat_Pos3],Explored_States)),
   transport([Mis_Left3,Can_Left3,Mis_Right3,Can_Right3,Boat_Pos3],[Mis_Left2,Can_Left2,Mis_Right2,Can_Right2,Boat_Pos2],
   [[Mis_Left3,Can_Left3,Mis_Right3,Can_Right3,Boat_Pos3]|Explored_States],[ [[Mis_Left3,Can_Left3,Mis_Right3,Can_Right3,Boat_Pos3],[Mis_Left1,Can_Left1,Mis_Right1,Can_Right1,Boat_Pos1]] | Moves ]).

transport([Mis_Left,Can_Left,Mis_Right,Can_Right,Boat_Pos],[Mis_Left,Can_Left,Mis_Right,Can_Right,Boat_Pos],_,Moves):-
	print_list1(Moves).
	

% Safe states when Missionaries >= Cannibals or No Missionaries.
safe_state(Mis_Left,Can_Left,Mis_Right,Can_Right):-
	Mis_Left >=0, Can_Left >=0, Mis_Right >= 0, Can_Right >= 0, (Mis_Left >= Can_Left; Mis_Left == 0), (Mis_Right >= Can_Right ; Mis_Right == 0).

% there can be 5 possible moves from one bank to other -
% 1. one cannibal + one missionary
% 2. one cannibal
% 3. one missionary
% 4. two cannibal
% 5. two missionary
% there are 2 banks, so total 10 possible moves are there.

%%%%%%%%%%%%%%%------Case 1--------%%%%%%%%%%%%%%%%%%%%%%%
%Case 1. One Cannibal + One Missionary left to right
move([Mis_Left,Can_Left,Mis_Right,Can_Right,left],[Mis_Left1,Can_Left1,Mis_Right1,Can_Right1,right]):-
		Can_Left1 is Can_Left - 1,
		Can_Right1 is Can_Right +1,
		Mis_Left1 is Mis_Left - 1,
		Mis_Right1 is Mis_Right +1,
		safe_state(Mis_Left1,Can_Left1,Mis_Right1,Can_Right1).

%Case 1. One Cannibal + One Missionary right to left
move([Mis_Left,Can_Left,Mis_Right,Can_Right,right],[Mis_Left1,Can_Left1,Mis_Right1,Can_Right1,left]):-
		Can_Left1 is Can_Left + 1,
		Can_Right1 is Can_Right - 1,
		Mis_Left1 is Mis_Left + 1,
		Mis_Right1 is Mis_Right - 1,
		safe_state(Mis_Left1,Can_Left1,Mis_Right1,Can_Right1).


%%%%%%%%%%%%%%%------Case 2--------%%%%%%%%%%%%%%%%%%%%%%%
%Case 2. One Cannibals left to right

move([Mis_Left,Can_Left,Mis_Right,Can_Right,left],[Mis_Left,Can_Left1,Mis_Right,Can_Right1,right]):-
		Can_Left1 is Can_Left - 1,
		Can_Right1 is Can_Right +1,
		safe_state(Mis_Left,Can_Left1,Mis_Right,Can_Right1).
		
%Case 2. One Cannibals right to left
move([Mis_Left,Can_Left,Mis_Right,Can_Right,right],[Mis_Left,Can_Left1,Mis_Right,Can_Right1,left]):-
		Can_Left1 is Can_Left + 1,
		Can_Right1 is Can_Right - 1,
		safe_state(Mis_Left,Can_Left1,Mis_Right,Can_Right1).

%%%%%%%%%%%%%%%------Case 3--------%%%%%%%%%%%%%%%%%%%%%%%
%Case 3. One Missionary left to right
move([Mis_Left,Can_Left,Mis_Right,Can_Right,left],[Mis_Left1,Can_Left,Mis_Right1,Can_Right,right]):-
		Mis_Left1 is Mis_Left - 1,
		Mis_Right1 is Mis_Right + 1,
		safe_state(Mis_Left1,Can_Left,Mis_Right1,Can_Right).
		
%Case 3. One Missionary right to left
move([Mis_Left,Can_Left,Mis_Right,Can_Right,right],[Mis_Left1,Can_Left,Mis_Right1,Can_Right,left]):-
		Mis_Left1 is Mis_Left + 1,
		Mis_Right1 is Mis_Right - 1,
		safe_state(Mis_Left1,Can_Left,Mis_Right1,Can_Right).

%%%%%%%%%%%%%%%------Case 4--------%%%%%%%%%%%%%%%%%%%%%%%		
%Case 4. Two Cannibals left to right
move([Mis_Left,Can_Left,Mis_Right,Can_Right,left],[Mis_Left,Can_Left1,Mis_Right,Can_Right1,right]):-
		Can_Left1 is Can_Left - 2,
		Can_Right1 is Can_Right +2,
		safe_state(Mis_Left,Can_Left1,Mis_Right,Can_Right1).
		
%Case 4. Two Cannibals right to left
move([Mis_Left,Can_Left,Mis_Right,Can_Right,right],[Mis_Left,Can_Left1,Mis_Right,Can_Right1,left]):-
		Can_Left1 is Can_Left + 2,
		Can_Right1 is Can_Right - 2,
		safe_state(Mis_Left,Can_Left1,Mis_Right,Can_Right1).

%%%%%%%%%%%%%%%------Case 5--------%%%%%%%%%%%%%%%%%%%%%%% 
%Case 5. Two Missionaries left to right
move([Mis_Left,Can_Left,Mis_Right,Can_Right,left],[Mis_Left1,Can_Left,Mis_Right1,Can_Right,right]):-
		Mis_Left1 is Mis_Left - 2,
		Mis_Right1 is Mis_Right + 2,
		safe_state(Mis_Left1,Can_Left,Mis_Right1,Can_Right).
		
%Case 5. Two Missionaries right to left
move([Mis_Left,Can_Left,Mis_Right,Can_Right,right],[Mis_Left1,Can_Left,Mis_Right1,Can_Right,left]):-
		Mis_Left1 is Mis_Left + 2,
		Mis_Right1 is Mis_Right - 2,
		safe_state(Mis_Left1,Can_Left,Mis_Right1,Can_Right).
		
		
% Printing

boat_position([_,_,_,_,E|[]],E).
move_count([_,_,Mis_Right1,Can_Right1,_],[_,_,Mis_Right2,Can_Right2,Pos],Count_Mis,Count_Can) :- Pos == right, 
						Count_Mis is Mis_Right2 - Mis_Right1, Count_Can is Can_Right2 - Can_Right1.
move_count([Mis_Left1,Can_Left1,_,_,_],[Mis_Left2,Can_Left2,_,_,Pos],Count_Mis,Count_Can) :- Pos == left, 
						Count_Mis is Mis_Left2 - Mis_Left1, Count_Can is Can_Left2 - Can_Left1.

						
print_list1(MovesList) :- reverse(MovesList,MovesList1),output(MovesList1,[m1,m2,m3],[c1,c2,c3],[],[]).						
						
output([],_,_,_,_) :- nl. 
output([[A,B]|MovesList],Left_Mis,Left_Can,Right_Mis,Right_Can) :-  
			boat_position(A,Pos1), Pos1 == right, move_count(B,A,Count_Mis,Count_Can), 
			move_list(Left_Mis,Right_Mis,Count_Mis,'',Left_Mis1,Right_Mis1,X1),
			move_list(Left_Can,Right_Can,Count_Can,'',Left_Can1,Right_Can1,X2),
			atom_concat(X2,X1,X), write(X),nl,
			output(MovesList,Left_Mis1,Left_Can1,Right_Mis1,Right_Can1).
			
output([[A,B]|MovesList],Left_Mis,Left_Can,Right_Mis,Right_Can) :-  
			boat_position(A,Pos1), Pos1 == left, move_count(B,A,Count_Mis,Count_Can), 
			move_list(Right_Mis,Left_Mis,Count_Mis,'',Right_Mis1,Left_Mis1,X1),
			move_list(Right_Can,Left_Can,Count_Can,'',Right_Can1,Left_Can1,X2),
			atom_concat(X2,X1,X), write(X),nl,
			output(MovesList,Left_Mis1,Left_Can1,Right_Mis1,Right_Can1).
			
			
move_list(From,To,0,OutString,From,To,OutString).
move_list([H1|T1],To,Count,OutString,From1,To1,OutString1) :- Count >0, Count1 is Count-1, 
				atom_concat(OutString,H1,X),append(To,[H1],To2),move_list(T1,To2,Count1,X,From1,To1,OutString1).
	
