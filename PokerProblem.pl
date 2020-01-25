%successors List
succ(straight_flush, four_of_a_kind).
succ(four_of_a_kind, full_house).
succ(full_house, straight).
succ(straight, flush).               
succ(flush, three_of_a_kind).
succ(three_of_a_kind, two_pair).     
succ(two_pair, one_pair).
succ(one_pair, no_pair).

succ(ace,king).
succ(king,queen).
succ(queen,jack).
succ(jack,10).      
succ(10,9).         
succ(9,8).
succ(8,7).          
succ(7,6).          
succ(6,5).
succ(5,4).          
succ(4,3).          
succ(3,2).


%Compare Values
greater_value(A,B) :- succ(A,X) , (X = B ; greater_value(X,B)).

%%%%%%%%%% determine hand of a user %%%%
determine_hand([card(X,A),card(X,B),card(X,C),card(X,D),card(X,E)], straight_flush) :- succ(E,D), succ(D,C), succ(C,B), succ(B,A),!.
determine_hand([card(_,A),card(_,B),card(_,B),card(_,B),card(_,C)], four_of_a_kind) :- A = B ; C = B,!.
determine_hand([card(_,A),card(_,B),card(_,C),card(_,D),card(_,E)], full_house) :-  A = B, D = E, (C = D ; C = B),!.
determine_hand([card(X,_),card(X,_),card(X,_),card(X,_),card(X,_)], flush) :- !.
determine_hand([card(_,A),card(_,B),card(_,C),card(_,D),card(_,E)], straight) :-  succ(E,D), succ(D,C), succ(C,B), succ(B,A), !.
determine_hand([card(_,A),card(_,B),card(_,C),card(_,D),card(_,E)], three_of_a_kind) :-  (A = B, B = C); (B = C, C = D); (C = D, D = E) , !.
determine_hand([card(_,A),card(_,A),card(_,B),card(_,B),card(_,_)], two_pair) :- !.
determine_hand([card(_,A),card(_,A),card(_,_),card(_,B),card(_,B)], two_pair) :- !.
determine_hand([card(_,_),card(_,A),card(_,A),card(_,B),card(_,B)], two_pair) :- !.
determine_hand([card(_,A),card(_,B),card(_,C),card(_,D),card(_,E)], one_pair) :-   A = B; B = C; C = D; D = E, !.
determine_hand(_,no_pair).

%Main Winnner Method
better_poker_hand(Hand1, Hand2, Hand) :-
  sort_hand(Hand1, Sorted_Hand1),
  sort_hand(Hand2, Sorted_Hand2),
  determine_hand(Sorted_Hand1,  X1),
  determine_hand(Sorted_Hand2,  X2),
  high_hand(X1, X2, Z),((Z=X1 , Hand = Hand1),!;(Z=X2 , Hand = Hand2),!;
  (Z = tie, tiebreak(X1,Sorted_Hand1,Sorted_Hand2,Z1),((Z1 = left , Hand = Hand1);(Z1 = right , Hand = Hand2)))).
  
  
%Tiebreak  For Three of a kind
tiebreak(three_of_a_kind, H1, H2, Winner) :- higher_middle_card(H1, H2, Winner).
higher_middle_card(H1, H2, Winner) :-
  H1 = [_,_,card(_,V1),_,_],
  H2 = [_,_,card(_,V2),_,_],
  high_hand(V1,V2,Higher),
  (Higher = V1, Winner = left;
   Higher = V2, Winner = right).

%Tiebreak For Straight Flush
tiebreak(straight_flush, H1, H2, Winner)  :- higher_last_card(H1, H2, Winner).
higher_last_card(H1,H2,Winner) :-
  H1 = [_,_,_,_,card(_,V1)],
  H2 = [_,_,_,_,card(_,V2)],
  high_hand(V1,V2,Higher),
  (Higher = V1, Winner = left ;
   Higher = V2, Winner = right).

%Tiebreak For four_of_a_kind 
tiebreak(four_of_a_kind, H1, H2, Winner)  :- higher_middle_card(H1, H2, Winner).

%Tiebreak For full_house
tiebreak(full_house, H1, H2, Winner)  :- higher_middle_card(H1, H2, Winner).

%Tiebreak For Straight
tiebreak(straight, H1, H2, Winner):- higher_last_card(H1, H2, Winner).

%Tiebreak for Flush
tiebreak(flush, H1, H2, Winner) :- tiebreak(no_pair, H1, H2, Winner).

%Tiebreak For Two pairs
tiebreak(two_pair, Hand1, Hand2, Winner) :-
  isolate_pairs(Hand1, [High1,_], [Low1,_], NoPair1),
  isolate_pairs(Hand2, [High2,_], [Low2,_], NoPair2),
  (wins_with_hand(High1, High2, Winner),
   Winner \= tie;
   wins_with_hand(Low1, Low2, Winner),
   Winner \= tie;
   wins_with_hand(NoPair1,NoPair2, Winner)).


isolate_pairs(Hand, High, Low, NoPair) :-
  [card(S1,V1),card(S2,V2),card(S3,V3),card(S4,V4),card(S5,V5)] = Hand,
  (V5 = V4, High = [card(S4,V4),card(S5,V5)],
    (V3 = V2, Low = [card(S3,V3),card(S2,V2)], NoPair = card(S1,V1) ;
     V1 = V2, Low = [card(S2,V2),card(S1,V1)], NoPair = card(S3,V3))) ;
  (High = [card(S3,V3),card(S4,V4)],
   Low = [card(S1,V1),card(S2,V2)],  
   NoPair = card(S5,V5)).   
   
wins_with_hand(Card1,Card2, Winner) :-
  wins(Card1, Card2, Card1), Winner = left ;
  wins(Card1, Card2, Card2), Winner = right ;
  Winner = tie.

  
%Tiebreak for One_Pair
tiebreak(one_pair, H1, H2, Winner) :-
  isolate_pair(H1, [PairCard1,_], Rst1),
  isolate_pair(H2, [PairCard2,_], Rst2),
  (wins_with_hand(PairCard1,PairCard2, Winner), Winner \= tie ;
   tiebreak(no_pair, Rst1, Rst2, Winner)).  

isolate_pair(Hand, Pair, Rst) :-
  [card(S1,V1),card(S2,V2),card(S3,V3),card(S4,V4),card(S5,V5)] = Hand,
  (V1 = V2, Pair = [card(S1,V1),card(S2,V2)], Rst = [card(S3,V3),card(S4,V4),card(S5,V5)] ;
   V2 = V3, Pair = [card(S2,V2),card(S3,V3)], Rst = [card(S1,V1),card(S4,V4),card(S5,V5)] ;
   V4 = V3, Pair = [card(S3,V3),card(S4,V4)], Rst = [card(S1,V1),card(S2,V2),card(S5,V5)] ;
   V4 = V5, Pair = [card(S4,V4),card(S5,V5)], Rst = [card(S1,V1),card(S2,V2),card(S3,V3)]).
  

%Tiebreak For No_Pair
tiebreak(no_pair, H1, H2, X) :- 
  reverse(H1, RevH1),
  reverse(H2, RevH2),
  highest_card_chain(RevH1, RevH2, X).
  
highest_card_chain([H1|T1], [H2|T2], X) :-
  wins(H1,H2,Z),
  (Z = H1, X = left ;
   Z = H2, X = right ;
   Z = tie, highest_card_chain(T1,T2,X)).
  


%Hand Sorting  
sort_hand([], []).
sort_hand([H|T], Sorted) :-
  filter(H,T,Lower,Higher),
  sort_hand(Lower,SortedLower),
  sort_hand(Higher,SortedHigher),
  append(SortedLower, [H|SortedHigher], Sorted).


filter(_, [], [], []).  
filter(Pivot, [H|T], [H|Lower], Higher) :-
  wins(Pivot,H,Z),
  (Z = Pivot ; Z = tie),
  filter(Pivot, T, Lower, Higher).
filter(Pivot, [H|T], Lower, [H|Higher]) :-
  wins(Pivot,H,H),
  filter(Pivot, T, Lower, Higher).


%Compare cards
wins(card(_,X),card(_,X),tie).
wins(card(D,X1),card(_,X2),card(D,X1)) :- greater_value(X1,X2).
wins(card(_,X1),card(D,X2),card(D,X2)) :- greater_value(X2,X1).

%Compare Card Values
high_hand(A,A,tie).
high_hand(A,B,Z) :- greater_value(A,B),Z = A.
high_hand(A,B,Z) :- greater_value(B,A),Z = B.