%Marriage Problem
%Stable Men checks if the combination is stable
stable_Men([], _, _).
stable_Men([M|Ms], Ws, Marriages):-
  stable_Women(Ws, M, Marriages),
  stable_Men(Ms, Ws, Marriages).
  
%Stable Women checks if the combination is stable
stable_Women([], _, _).
stable_Women([W|Ws], M, Marriages):-
  not(unstable(M, W, Marriages)),
  stable_Women(Ws, M, Marriages).

%Stable relation if it is not Unstable  
unstable(M, W, Marriages):-
  married(M, Wife, Marriages),
  married(Husband, W, Marriages),
  prefers(M, W, Wife),
  prefers(W, M, Husband).
  
married(M, W, Marriages):-
  rest([M, W], Marriages, _). 

prefers(Person, OtherPerson, Spouse):-
  preference(Person, Preference),
  rest(OtherPerson, Preference, Rest),
  rest(Spouse, Rest, _).

rest(X, [X|Ys], Ys):-!.
rest(X, [_|Ys], Zs):-rest(X, Ys, Zs).

select1(X, [X|Ys], Ys).
select1(X, [Y|Ys], [Y|Zs]):-select1(X, Ys, Zs).

test(Ms, Ws, Marriages):-
  generate(Ms, Ws, Marriages),
  stable_Men(Ms, Ws, Marriages).
  
generate([], [], []).
generate([M|Ms], Ws, [[M,W]|Marriages]):-
  select1(W, Ws, Ws1),
  generate(Ms, Ws1, Marriages).
  
%Main Call. Takes Preferences and Provide the Output
stable_marriage(Men_and_Preferences,Women_and_Preferences, Output) :- get_list_people(Men_and_Preferences,Men),
			get_list_people(Women_and_Preferences,Women),value_assert(Men_and_Preferences),value_assert(Women_and_Preferences),
			test(Men,Women,Output).

%Get List of People Men and Women			
get_list_people([],[]) :- !.
get_list_people([Term|Tail],[H|T]) :- Term =.. [_|[H|_]], get_list_people(Tail,T).

%Assert Facts
value_assert([]):- !.
value_assert([Term|Tail]) :- assert(Term),value_assert(Tail).
