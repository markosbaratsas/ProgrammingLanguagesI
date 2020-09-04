string_to_answer(List,String):-
  reverse(List,List1),
  string_to_list(String,List1).

state_not_exists(state(FirstStack,FirstChar,LastChar, Bool, _,ExistsA,ExistsC,ExistsG,ExistsU)) :-
  state_exists(state(FirstStack,FirstChar,LastChar, Bool, _,ExistsA,ExistsC,ExistsG,ExistsU))->false;true.

state_exists(state(FirstStack,FirstChar,LastChar, Bool, _,ExistsA,ExistsC,ExistsG,ExistsU)) :-
  call(state(FirstStack1,FirstChar1,LastChar1, Bool1, _,ExistsA1,ExistsC1,ExistsG1,ExistsU1)),
  FirstStack = FirstStack1,
  FirstChar = FirstChar1,
  LastChar = LastChar1,
  Bool = Bool1,
  ExistsA = ExistsA1,
  ExistsC = ExistsC1,
  ExistsG = ExistsG1,
  ExistsU = ExistsU1.


produce_with_c(state(FirstStack,FirstChar,LastChar, Bool, SoFarPathString,ExistsA,ExistsC,ExistsG,ExistsU), Answer, BoolFoundAnswer, RestStates, ReturningList):-
(
  SoFarPathString = [C | _TheRest],
  /* We don't want to have '.....rr' as a state, so we blacklist it out */
  (C == 'c' -> (Cont = 0, BoolFoundAnswer=0, ReturningList = RestStates)
  ;(
    length(SoFarPathString,LengthSoFarPathString),
    LengthSoFarPathString > 5 -> (
      SoFarPathString = [X,Y|_TheRest1],
      /* We don't want to have '.....crcr' as a state, so we blacklist it out */
      ((X == 'r', Y == 'c') -> (Cont = 0, BoolFoundAnswer=0, ReturningList = RestStates)
      ;( Cont = 1 ))
    )
  ;(Cont = 1))),

  (Cont =:= 1 -> (

    NewSoFarPathString = ['c'|SoFarPathString],

    (Bool =:= 0 -> NewBool = 1; NewBool = 0),
    /* Check if it already exists*/
    (state_exists(state(FirstStack,FirstChar,LastChar, NewBool, NewSoFarPathString,ExistsA,ExistsC,ExistsG,ExistsU)) -> (BoolFoundAnswer=0, ReturningList = RestStates)
    ;(

      append(RestStates,[state(FirstStack,FirstChar,LastChar, NewBool, NewSoFarPathString,ExistsA,ExistsC,ExistsG,ExistsU)], ReturningList),
      assert(state(FirstStack,FirstChar,LastChar, NewBool, NewSoFarPathString,ExistsA,ExistsC,ExistsG,ExistsU)),

      /* Checking if this is the last state */
      length(FirstStack,Length),
      (Length =:= 0 -> (Answer = NewSoFarPathString, BoolFoundAnswer=1)
      ;(BoolFoundAnswer=0))
    ))
  );(BoolFoundAnswer=0, ReturningList = RestStates))
).


produce_with_p(state([First|Rest], FirstChar, LastChar, Bool, SoFarPathString,ExistsA,ExistsC,ExistsG,ExistsU), Answer, BoolFoundAnswer, RestStates, ReturningList):-
(

  (Bool =:= 1 -> (
    First == 'U' -> FirstWeUse = 'A' ;
    First == 'A' -> FirstWeUse = 'U' ;
    First == 'G' -> FirstWeUse = 'C' ;
    First == 'C' -> FirstWeUse = 'G'
  );(
    FirstWeUse = First
  )),

  /* Here we try to understand if it the char FirstWeUse exists in the second stack*/
  (
    FirstWeUse == 'A' -> (ExistsA =:= 0 -> NewOne = 'A';(NewOne = 'N'));
    FirstWeUse == 'C' -> (ExistsC =:= 0 -> NewOne = 'C';(NewOne = 'N'));
    FirstWeUse == 'G' -> (ExistsG =:= 0 -> NewOne = 'G';(NewOne = 'N'));
    FirstWeUse == 'U' -> (ExistsU =:= 0 -> NewOne = 'U';(NewOne = 'N'))
  ),

  /* if(exists.contains(m) && first != m) b = true; */
  (NewOne == 'N'-> (
    (FirstWeUse \= FirstChar) -> (B is 1)
    ; (B is 0)
  );(B is 0)),

  /* Like the Java program */
  ((B =:= 0) -> (

    (NewSoFarPathString = ['p'|SoFarPathString]),

    /* Check if it already exists*/
    (
      (NewOne == 'N', state_not_exists(state(Rest,FirstWeUse,LastChar, Bool, NewSoFarPathString,ExistsA,ExistsC,ExistsG,ExistsU))) -> (
          append(RestStates,[state(Rest,FirstWeUse,LastChar, Bool, NewSoFarPathString,ExistsA,ExistsC,ExistsG,ExistsU)], ReturningList),
          assert(state(Rest,FirstWeUse,LastChar, Bool, NewSoFarPathString,ExistsA,ExistsC,ExistsG,ExistsU)),
          AddedState = 1
      );
      (NewOne == 'A', state_not_exists(state(Rest,FirstWeUse,LastChar, Bool, NewSoFarPathString,1,ExistsC,ExistsG,ExistsU))) -> (
        append(RestStates,[state(Rest,FirstWeUse,LastChar, Bool, NewSoFarPathString,1,ExistsC,ExistsG,ExistsU)], ReturningList),
        assert(state(Rest,FirstWeUse,LastChar, Bool, NewSoFarPathString,1,ExistsC,ExistsG,ExistsU)),
        AddedState = 1
      );
      (NewOne == 'C', state_not_exists(state(Rest,FirstWeUse,LastChar, Bool, NewSoFarPathString,ExistsA,1,ExistsG,ExistsU))) -> (
        append(RestStates,[state(Rest,FirstWeUse,LastChar, Bool, NewSoFarPathString,ExistsA,1,ExistsG,ExistsU)], ReturningList),
        assert(state(Rest,FirstWeUse,LastChar, Bool, NewSoFarPathString,ExistsA,1,ExistsG,ExistsU)),
        AddedState = 1
      );
      (NewOne == 'G', state_not_exists(state(Rest,FirstWeUse,LastChar, Bool, NewSoFarPathString,ExistsA,ExistsC,1,ExistsU))) -> (
        append(RestStates,[state(Rest,FirstWeUse,LastChar, Bool, NewSoFarPathString,ExistsA,ExistsC,1,ExistsU)], ReturningList),
        assert(state(Rest,FirstWeUse,LastChar, Bool, NewSoFarPathString,ExistsA,ExistsC,1,ExistsU)),
        AddedState = 1
      );
      (NewOne == 'U', state_not_exists(state(Rest,FirstWeUse,LastChar, Bool, NewSoFarPathString,ExistsA,ExistsC,ExistsG,1))) -> (
        append(RestStates,[state(Rest,FirstWeUse,LastChar, Bool, NewSoFarPathString,ExistsA,ExistsC,ExistsG,1)], ReturningList),
        assert(state(Rest,FirstWeUse,LastChar, Bool, NewSoFarPathString,ExistsA,ExistsC,ExistsG,1)),
        AddedState = 1
      );
      (BoolFoundAnswer=0, ReturningList = RestStates, AddedState = 0)
    ),

    (AddedState =:= 1 -> (
      /* Checking if this is the last state */
      length(Rest,Length),
      ((Length =:= 0) -> ( Answer = NewSoFarPathString, BoolFoundAnswer = 1)
      ;(BoolFoundAnswer = 0))
    );(true))
  );(BoolFoundAnswer=0, ReturningList = RestStates))
).

produce_with_r(state(FirstStack,FirstChar,LastChar, Bool, SoFarPathString,ExistsA,ExistsC,ExistsG,ExistsU), Answer, BoolFoundAnswer, RestStates, ReturningList):-
(

  SoFarPathString = [C | _TheRest],
  /* We don't want to have '.....rr' as a state, so we blacklist it out */
  (C == 'r' -> (Cont = 0, BoolFoundAnswer=0, ReturningList = RestStates)
  ;(
    length(SoFarPathString,LengthSoFarPathString),
    LengthSoFarPathString > 5 -> (
      SoFarPathString = [X,Y|_TheRest1],
      /* We don't want to have '.....crcr' as a state, so we blacklist it out */
      ((X == 'c', Y == 'r') -> (Cont = 0, BoolFoundAnswer=0, ReturningList = RestStates)
      ;( Cont = 1 ))
    )
  ;(Cont = 1))),

  (Cont =:= 1 -> (
    (NewSoFarPathString = ['r'|SoFarPathString]),

    /* Check if it already exists*/
    (state_exists(state(FirstStack,LastChar,FirstChar, Bool, NewSoFarPathString,ExistsA,ExistsC,ExistsG,ExistsU)) -> (BoolFoundAnswer=0, ReturningList = RestStates)
    ;(

      append(RestStates,[state(FirstStack,LastChar,FirstChar, Bool, NewSoFarPathString,ExistsA,ExistsC,ExistsG,ExistsU)], ReturningList),
      assert(state(FirstStack,LastChar,FirstChar, Bool, NewSoFarPathString,ExistsA,ExistsC,ExistsG,ExistsU)),

      /* Checking if this is the last state */
      length(FirstStack,Length),
      (Length =:= 0 -> (Answer = NewSoFarPathString, BoolFoundAnswer=1)
      ;(BoolFoundAnswer=0))
    ))
  );(BoolFoundAnswer=0, ReturningList = RestStates))
).

next_states(Answer, BoolFoundAnswer, [FirstState | RestStates], ReturningList):-
(
    produce_with_c(FirstState, Answer, BoolFoundAnswer1, RestStates, ReturningList1),
    (BoolFoundAnswer1 =\= 1 -> (
      produce_with_p(FirstState, Answer, BoolFoundAnswer2, ReturningList1, ReturningList2),
      (BoolFoundAnswer2 =\= 1 -> (
        produce_with_r(FirstState, Answer, BoolFoundAnswer3, ReturningList2, ReturningList3)
      );true)
    );true),


    retractall(FirstState),

    (BoolFoundAnswer1 =:= 1 -> ReturningList = ReturningList1;
    BoolFoundAnswer2 =:= 1 -> ReturningList = ReturningList2;
    BoolFoundAnswer3 =:= 1 -> ReturningList = ReturningList3;
    ReturningList = ReturningList3),

    ((BoolFoundAnswer1 =:= 1 ; BoolFoundAnswer2 =:= 1  ; BoolFoundAnswer3 =:= 1) -> (BoolFoundAnswer=1)
    ;(BoolFoundAnswer=0))
).


while(Answer, BoolFoundAnswer, SoFarList) :-
(
  BoolFoundAnswer =:= 1 -> (true)
  ; (
    once(next_states(Answer, NewBoolFoundAnswer, SoFarList, NewSoFarList)),
    while(Answer, NewBoolFoundAnswer, NewSoFarList)
  )
).

/* state is like this:
 * state(FirstStack, FirstCharOfSecondStack,LastCharOfSecondStack, ComplementBool, SoFarPathString, ExistsA, ExistsC, ExistsG, ExistsU)  */
solve([FirstChar|Rest], Answer):-
  (
    FirstChar == 'A'-> (State = state(Rest,FirstChar,FirstChar, 0, ['p'],1,0,0,0));
    FirstChar == 'C'-> (State = state(Rest,FirstChar,FirstChar, 0, ['p'],0,1,0,0));
    FirstChar == 'G'-> (State = state(Rest,FirstChar,FirstChar, 0, ['p'],0,0,1,0));
    FirstChar == 'U'-> (State = state(Rest,FirstChar,FirstChar, 0, ['p'],0,0,0,1))
  ),
  assert(State),
  while(ListAnswer, 0, [State]),
  string_to_answer(ListAnswer,Answer).

solveAll(Stream, N, Answers, RealAnswer) :-
(
  N =:= 0->(
          RealAnswer = Answers
  );(
  read_line(Stream, [List1]),
  reverse(List1,List),
  % write("List: "),  writeln(List),
  retractall(state(_,_,_, _, _,_,_,_,_)),
  once(solve(List, Answer)),
  NewN is (N-1),
  % write("Answer: "),  writeln(Answer),
	append(Answers, [Answer], NewAnswers),
  solveAll(Stream, NewN, NewAnswers, RealAnswer)
  )
).

vaccine(File,Answer) :-
  open(File, read, Stream),
  read_number(Stream, [N]),
  solveAll(Stream, N, [], Answer).

read_line(Stream, L) :-
  read_line_to_codes(Stream, Line),
  atom_codes(Atom, Line),
  atomic_list_concat(Atoms, ' ', Atom),
  maplist(atom_chars, Atoms, L).

read_number(Stream, L) :-
  read_line_to_codes(Stream, Line),
  atom_codes(Atom, Line),
  atomic_list_concat(Atoms, ' ', Atom),
  maplist(atom_number, Atoms, L).
