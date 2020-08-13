divide(L,L1,L2) :-
	length(L, LengthL),
	LengthL1 is div(LengthL,2),
	length(L1, LengthL1),
	append(L1,L2,L).
	

% mergesort link: http://kti.ms.mff.cuni.cz/~bartak/prolog/sorting.html
merge_sort([],[]).     % empty list is already sorted
merge_sort([X],[X]).   % single element list is already sorted
merge_sort(List,Sorted):-
    List=[_,_|_],divide(List,L1,L2),     % list with at least two elements is divided into two parts
	merge_sort(L1,Sorted1),merge_sort(L2,Sorted2),  % then each part is sorted
	merge(Sorted1,Sorted2,Sorted).                  % and sorted parts are merged
merge([],L,L).
merge(L,[],L):-L\=[].
merge([X|T1],[Y|T2],[X|T]):-X=<Y,merge(T1,[Y|T2],T).
merge([X|T1],[Y|T2],[Y|T]):-X>Y,merge([X|T1],T2,T).

replace_in_list(Es, N, X, Xs) :-
   	same_length(Es, Xs),
   	append(Prefix, [_|Suffix], Es),
   	length(Prefix, N),
   	append(Prefix, [X|Suffix], Xs),
   	!.

dfs_while_loop(Cur, CycleNumber, U) :-
(
	Cur =:= U -> true
	;
	(
		b_getval(par, Par),
		arg(Cur, Par, ParCur),
		NewCur is ParCur,

		b_getval(mark, Mark),
		setarg(NewCur, Mark, CycleNumber),
		dfs_while_loop(NewCur, CycleNumber, U)
	)
).


dfs_for_loop([V|RestList], U, CycleNumber, ReturningCycleNumber2) :-
(
	length(RestList,L),
       	L =:= 0 -> (
		b_getval(par, Par),
		arg(U, Par, ParU),
		(ParU =:= V -> (ReturningCycleNumber2 = CycleNumber)
		;
			dfs_cycle(V, U, CycleNumber, ReturningCycleNumber),
			ReturningCycleNumber2 = ReturningCycleNumber)
	)
	;
	(
		b_getval(par, Par),
		arg(U, Par, ParU),
		(ParU =:= V -> dfs_for_loop(RestList, U, CycleNumber, ReturningCycleNumber2)
		;
			dfs_cycle(V, U, CycleNumber, ReturningCycleNumber),
			dfs_for_loop(RestList, U, ReturningCycleNumber, ReturningCycleNumber2))
	)
).


dfs_cycle(U, P, CycleNumber, ReturningCycleNumber) :-
(
	b_getval(color,Color),
	arg(U,Color,2) -> ReturningCycleNumber is CycleNumber
	;
	(
		b_getval(color,Color),
		arg(U,Color,1) -> 
		(
			NewCycleNumber is (CycleNumber+1),
			Cur is P,

			b_getval(mark,Mark),
			setarg(Cur,Mark,NewCycleNumber),

			dfs_while_loop(Cur, NewCycleNumber, U),
			ReturningCycleNumber is NewCycleNumber
		)
		;
		(
			b_getval(par, Par),
			setarg(U, Par, P),

			b_getval(color,Color),
			setarg(U, Color, 1),

			b_getval(graph, Graph),
			arg(U, Graph, GraphUList),
					
			dfs_for_loop(GraphUList, U, CycleNumber, ReturningCycleNumber2),

			setarg(U, Color, 2),

			ReturningCycleNumber is ReturningCycleNumber2
		)
	)

).
       
printingGraph_for_loop1(Ni, CountLength, Length, CycleList, ReturningCycleList) :-
(
	Ni =:= 0 -> 
		Length is CountLength,
		ReturningCycleList = CycleList		
	;
	(
		(
		b_getval(mark, Mark),
		arg(Ni, Mark, MarkNi),
		(MarkNi =:= 1) -> 
			NewCountLength is (CountLength + 1),
			NewNi is (Ni-1),
			printingGraph_for_loop1(NewNi, NewCountLength, Length, [Ni|CycleList], ReturningCycleList)
		;
			NewCountLength is CountLength,
			NewNi is (Ni-1),
			printingGraph_for_loop1(NewNi, NewCountLength, Length, CycleList, ReturningCycleList)
		)
	)
).

findNumberNodes_for_loop([V|RestList],CountNodes,NewCountNodes, X) :-
(	
	length(RestList, L),
	L =:= 0 -> (
		(V =:= X -> 
			NewCountNodes is CountNodes
		;
			findNumberNodes(V,CountNodes,ReturningCountNodes, X),
			NewCountNodes = ReturningCountNodes
		)
	)
	;(	
		(V =:= X -> 
			% continue
			findNumberNodes_for_loop(RestList,CountNodes,NewCountNodes, X) 
		;
			findNumberNodes(V,CountNodes,ReturningCountNodes, X),
			findNumberNodes_for_loop(RestList,ReturningCountNodes,NewCountNodes, X) 
		)
	)
).


findNumberNodes(U,CountNodes,ReturningCountNodes, X) :-
(
	b_getval(color, Color),
	arg(U,Color,ColorU),
	(ColorU =:= 2) -> (
		NewCountNodes is (CountNodes + 1),

		setarg(U,Color,3),

		b_getval(graph, Graph),
		arg(U,Graph,GraphUList),

		findNumberNodes_for_loop(GraphUList,NewCountNodes,NewNewCountNodes, X),
		ReturningCountNodes = NewNewCountNodes
	)
	;(
		ReturningCountNodes = CountNodes
	)
).


printingGraph_for_loop3([Node|RestList], CountNodes, ReturningCountNodes, X) :-
(	
	length(RestList, L),
	L =:= 0 -> (
		b_getval(mark, Mark),
		arg(Node, Mark, MarkNode),
		MarkNode =:= 1 ->
			ReturningCountNodes = CountNodes
		;(
			findNumberNodes(Node,CountNodes,NewCountNodes, X),
			ReturningCountNodes = NewCountNodes
		)
	)
	;(
		b_getval(mark, Mark),
		arg(Node, Mark, MarkNode),
		MarkNode =:= 1 ->
			printingGraph_for_loop3(RestList, CountNodes, ReturningCountNodes, X)
		;(
			findNumberNodes(Node,CountNodes,NewCountNodes, X),
			printingGraph_for_loop3(RestList, NewCountNodes, ReturningCountNodes, X)
		)
	)	
).		

% I passed as a parameter a counter I and not cycle so that I don't "carry in my back"
% the whole List (because it might be heavy...)
printingGraph_for_loop2(I, [GraphX|RestList], CountNumberNodesList, ReturningCountNumberNodesList) :-
(
	I =:= 0 -> 
	(
		b_getval(graph, Graph),
		arg(GraphX,Graph,GraphXList),

                printingGraph_for_loop3(GraphXList, 1, CountNodes, GraphX),

		ReturningCountNumberNodesList = [CountNodes|CountNumberNodesList]
	)
	;(
		b_getval(graph, Graph),
		arg(GraphX,Graph,GraphXList),

       		printingGraph_for_loop3(GraphXList, 1, CountNodes, GraphX),

		NewI is (I - 1),
		printingGraph_for_loop2(NewI, RestList, [CountNodes|CountNumberNodesList], ReturningCountNumberNodesList)
	)
).


%%%%%%%%%%%%%%%%%%%% dokimase na peraseis ta mark, par san orismata stis sinartiseis
printingGraph(Ni, Answer) :-
(
       	printingGraph_for_loop1(Ni, 0, LengthCycle, [], CycleList),

	LengthCycleMinusOne is (LengthCycle - 1),
       	printingGraph_for_loop2(LengthCycleMinusOne, CycleList, [], ReturningCountNumberNodesList),

	merge_sort(ReturningCountNumberNodesList,Sorted),
	
	Answer = [LengthCycle|[Sorted]]
).

isCoherent(Ni, Bool) :-
(
	(Ni =:= 0 -> Bool is 1
	;
	(
		b_getval(color,Color),
		arg(Ni, Color, ColorNi),
		ColorNi =:= 0 -> Bool is 0
		;(	
			NewNi is (Ni-1),
			isCoherent(NewNi, Bool)
		)
	))	
).

% basically int main from c++ file
solve(Stream, Ni, Mi, Answer) :-
(	%if Ni!=Mi "NO CORONA"
	Ni =\= Mi ->
		% read the graph but not save the edges 
        	fake_read_graph(Stream, Mi),
		Answer = "'NO CORONA'"
	;
	(
		% else read graph
		read_graph(Stream, Mi),

		dfs_cycle(1, 0, 0, CycleNumber),

		isCoherent(Ni, CoherentBool),

		(CoherentBool =:= 0 -> 
			Answer = "'NO CORONA'"
		;(
			CycleNumber =:= 1 -> (
				printingGraph(Ni, Answer)
			)
			;(
				Answer = "'NO CORONA'"
			)
		)
		)
	)
).

create_graph(Ni, Graph, Mark, Color, Par) :-
(       Ni =< 0 -> (
		b_setval(graph,Graph), 
		b_setval(mark,Mark), 
		b_setval(color,Color), 
		b_setval(par,Par)
	)			
        ;(      
		setarg(Ni, Graph, [0]),
		setarg(Ni, Mark, 0),
		setarg(Ni, Color, 0),
		setarg(Ni, Par, 0),
                NewNi is (Ni - 1),
                create_graph(NewNi, Graph, Mark, Color, Par)
	)
).

fake_read_graph(Stream, Mi) :-
	(
	Mi =:= 0 -> true
	;
        (
		read_line(Stream, [_, _]),
		NewMi is (Mi - 1),
		fake_read_graph(Stream,NewMi)
	)
	).



read_graph(Stream, Mi) :-
(
	Mi =:= 0 -> true
	;
        (
		read_line(Stream, [Node1, Node2]),
		b_getval(graph, Graph),
		arg(Node1, Graph, Element1),
		length(Element1, L1),
                (L1 =:= 1 ->
			(Element1 =:= [0] ->
				setarg(Node1, Graph, [Node2])
			;	setarg(Node1, Graph, [Node2|Element1])
			)
		;	setarg(Node1, Graph, [Node2|Element1])
		),
		arg(Node2, Graph, Element2),
		length(Element2, L2),
                (L2 =:= 1 ->
			(Element2 = [0] -> 
				setarg(Node2, Graph, [Node1])
			;	setarg(Node2, Graph, [Node1|Element2])
			)
		;	setarg(Node2, Graph, [Node1|Element2])
		),
		NewMi is (Mi - 1),
		read_graph(Stream,NewMi)
	)
).
	


solveAll(Stream, N, I, Answers, RealAnswer) :-
(       N =:= I->
        (
                RealAnswer = Answers
        )
;       (
        read_line(Stream, [Ni, Mi]),
	% creating graph[][], mark[], color[], par[]
	NiPlusOne is Ni+1,
	functor(Graph, arrayGraph, NiPlusOne),
	functor(Mark, arrayMark, NiPlusOne),
	functor(Color, arrayColor, NiPlusOne),
	functor(Par, arrayPar, NiPlusOne),
	create_graph(Ni, Graph, Mark, Color, Par),
        once(solve(Stream, Ni, Mi, Answer)),
        NewI is (I+1),
	append(Answers, [Answer],NewAnswers),
        once(solveAll(Stream, N, NewI, NewAnswers, RealAnswer))
        )
).

coronograph(File,Answers) :-
        open(File, read, Stream),
        read_line(Stream, [NumberOfGraphs]),
        solveAll(Stream, NumberOfGraphs, 0, [], Answers).

read_line(Stream, L) :-
        read_line_to_codes(Stream, Line),
        atom_codes(Atom, Line),
        atomic_list_concat(Atoms, ' ', Atom),
        maplist(atom_number, Atoms, L).
