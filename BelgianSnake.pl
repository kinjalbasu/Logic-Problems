%%%%%% Belgian Snake Problem  %%%%%%%%%%%%%
%snake(Pattern,Rows,Cols): Print Belgian snake Pattern for the Rows and Cols count
snake(Pattern,Rows,Cols) :- front(Pattern,Rows,Cols,Pattern,Cols).
%Print forward path of the Patter
front(_,[],_,_,_).
front([],Rows,Cols,Pattern,Cols1) :- front(Pattern,Rows,Cols,Pattern,Cols1).
front([H|T],Rows,[_|T1],Pat,Cols) :- write(H),front(T,Rows,T1,Pat,Cols).
front(Pattern, [_|T1],[],Pat,Cols) :- nl, back(Pattern,T1,Cols,Pat,Cols,[]). 

%Print Reverse Pattern for Even rows
back(_,[],_,_,_,_).
back([],Rows,Cols,Pattern,Cols1,Rev) :- back(Pattern,Rows,Cols,Pattern,Cols1,Rev).
back([H|T],Rows,[_|T1],Pat,Cols,Rev) :- append([H],Rev,Rev1), back(T,Rows,T1,Pat,Cols,Rev1).
back(Pattern, [_|T1],[],Pat,Cols,Rev) :- print_list(Rev),nl, front(Pattern,T1,Cols,Pat,Cols).  

%Reverse printing list
print_list([H|T]) :- write(H), print_list(T).
print_list([]).