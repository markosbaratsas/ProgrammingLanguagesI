replaceInList([_|T], 0, X, [X|T]).
replaceInList([H|T], I, X, [H|R]):- I > -1, NI is I-1, replaceInList(T, NI, X, R), !.
replaceInList(L, _, _, L).

cutLastElement([H|_], I, Len1, List) :-	I =:= (Len1-1),	List = [H], !.
cutLastElement([H|T], I, Len1, [H|R]):- I < Len1, NI is I+1, cutLastElement(T, NI, Len1, R), !.
cutLastElement(L, _, _, L).


doItBinary(N, K_original, AnswerK_original, List, Answer) :-
(	N > 0 -> (
		(mod(N,2) =:= 1 ->
			NewK_original is (K_original + 1),
			K0 is 1
		;	NewK_original is K_original,
			K0 is 0
		),
		append(List, [K0], NewList),
		NewN is div(N,2), 
		doItBinary(NewN, NewK_original, AnswerK_original, NewList, Answer)
	)
	;	Answer = List,
		AnswerK_original = K_original
).

doTheInsideLoop(List, X, FinalList) :-
	XMinusOne is (X-1),
	nth0(X, List, Bx),
	nth0(XMinusOne, List, BxMinusOne),
	length(List,Len),
	Len1 is (Len-1),

	(((Bx > 0), (X<Len1)) ->
		NewBx is (Bx - 1),
		NewBxMinusOne is (BxMinusOne + 2), 
		replaceInList(List, X, NewBx,NewList),
		replaceInList(NewList, XMinusOne, NewBxMinusOne, NewNewList),
		FinalList = NewNewList
	;((Bx =:= 1), (X =:= Len1)) ->
		NewBxMinusOne is (BxMinusOne + 2), 
		replaceInList(List, XMinusOne, NewBxMinusOne, NewList),
		cutLastElement(NewList, 0, Len1, NewNewList),
		FinalList = NewNewList
	;((Bx > 1), (X =:= Len1)) ->
		NewBx is (Bx - 1),
		NewBxMinusOne is (BxMinusOne + 2), 
		replaceInList(List, X, NewBx,NewList),
		replaceInList(NewList, XMinusOne, NewBxMinusOne, NewNewList),
		FinalList = NewNewList
	;	NewX is (X+1),
		doTheInsideLoop(List, NewX, FinalList)
	).


doTheLoop(0, List, Answer) :- Answer = List. 
doTheLoop(Count, List, Answer) :-
	doTheInsideLoop(List, 1, FinalList),
	NewCount is (Count - 1),
	doTheLoop(NewCount, FinalList, Answer), !.


solve(Number,K,Answer) :-
	doItBinary(Number, 0, K_original, [], List),
	
	(((K > Number); (K < K_original)) ->
                        Answer = []
		;       Count is (K - K_original),
			doTheLoop(Count, List, Ready),
			Answer = Ready
                ), ! .

solveAll(List, N, I, Answers,RealAnswer) :-
(	N =:= I->
	(	
		RealAnswer = Answers
	)
;	(
	CurrentI1 is (2*I),
	CurrentI2 is (2*I+1),
	nth0(CurrentI1,List,CurrentNumber),
	nth0(CurrentI2,List,CurrentK),
	solve(CurrentNumber,CurrentK,Answer),
	append(Answers, [Answer], NewAnswers),
	NewI is (I+1),
	solveAll(List, N, NewI, NewAnswers,RealAnswer)
	)
).

powers2(File,Answers) :-
	read_input(File, N, List),
	solveAll(List, N, 0,[], Answers).

read_input(File, N, List) :-
    	open(File, read, Stream),
    	read_line(Stream, [N]),
	create_list(Stream, N, [], List).

create_list(Stream, N, List, Answer) :-
(	N =< 0 ->
	(
		Answer = List
	)
	;(	read_line(Stream, NumberK),
		append(List, NumberK, NewList),
		NewN is (N-1),
		create_list(Stream,NewN,NewList, Answer))
).

read_line(Stream, L) :-
	read_line_to_codes(Stream, Line),
	atom_codes(Atom, Line),
   	atomic_list_concat(Atoms, ' ', Atom),
    	maplist(atom_number, Atoms, L).
