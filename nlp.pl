consult(wn_s).

% (noun) is/are (adjectives)
sentence(T0,T4,Ind):-
	det(T0,T1,Ind),
	has_noun(T1,T2,Ind),
	is_phase(T2,T3),
	adj(T3,T4,Ind).



question([who,is | T0],T1,Ind) :-
    noun_phrase(T0,T1,Ind).
question([who,is | T0],T1,Ind) :-
    adjectives(T0,T1,Ind).
question([what,is | T0],T1,Ind) :-
    noun_phrase(T0,T1,Ind).
question([what,is | T0],T1,Ind) :-
    adjectives(T0,T1,Ind).

noun_phrase(T0,T3,Ind) :-
    det(T0,T1,Ind),
    adjectives(T1,T2,Ind),
    noun(T2,T3,Ind).

det([D | T],T,_):-
	member(D,[a,the]).

det(T,T,_).

adjectives(T0,T2,Ind) :-
    adj(T0,T1,Ind),
    adjectives(T1,T2,Ind).
adjectives(T,T,_).


noun([N|T],T,Ind):- 
	s(_,_,N,n,_,_),
	prop(N,subclass,Ind).

is_phase([is|T],T).

has_noun([N|T],T,N):-
	is_noun(N).

is_noun(N):-
	prop(_,subclass,N).
is_noun(N):-
	prop(N,subclass,_).
is_noun(N):-
	s(_,_,N,n,_,_).

/*
noun([Ind|T],T,Ind) :- 
	cs(_,_,N,n,_,_),
	prop(N,subclass,Ind).
*/

adj([A|T],T,Ind):-
	s(_,_,A,a,_,_),
	prop(Ind,is,A).

adv(R):-
	s(_,_,R,a,_,_).


%A(noun) is a B(noun)
prop(course,subclass,cs312).
prop(course,subclass,cs322).
prop(course,subclass,math315).



prop(cs312,department,comp_sci).
prop(cs322,department,comp_sci).
prop(math315,department,math).

prop(john,enrolled_in,cs312).
prop(mary,enrolled_in,cs312).
prop(jane,enrolled_in,math315).
prop(sally,enrolled_in,cs322).
prop(sam,enrolled_in,math315).

%A(noun) is B(adj)
prop(cs312,is,hard).

prop(mary,is,female).
prop(jane,is,female).
prop(sally,is,female).
prop(john,is,male).

prop(mary,is,tall).
prop(jane,is,tall).
prop(john,is,tall).
prop(jordan,is,tall).

prop(john,is,smart).
prop(sally,is,beautiful).

prop(animal, subclass,elephant).
prop(elephant,is,large).

prop(student,subclass,mary).
prop(student,subclass,jane).
prop(student,subclass,sally).
prop(student,subclass,john).
prop(student,subclass,sam).
prop(student,subclass,chris).

prop(person,subclass,A):-
	prop(student,subclass,A).

prop(object,subclass,A):-
	prop(course,subclass,A).
prop(object,subclass,A):-
	prop(animal,subclass,A).



/*
prop(A,subclass,C):-
	prop(A,subclass,B),
	prop(B,subclass,C).
*/
passed(S,C):-
    grade(S,C,G),
    G >= 50.

grade(sam,cs312,93).
grade(chris,cs312,82).

form_sentence_from_question([_,is|T],Ind,Ans):-
	atomic_list_concat([i,think,Ind,is|T], ' ', Ans).


startNLP(Ans) :-
    write("what do you want to ask?"),nl,flush_output(current_output),
    readln(Ln),
    distinct(process(Ln,Ans)).

%if question is true about somthing
process(Ln,Ans):-
    question(Ln,End,Ind),
    form_sentence_from_question(Ln,Ind,Ans),
    member(End,[[],['?'],['.']]).


process(Ln,Ans):-
    sentence(Ln,End,Ans),
    member(End,[[],['?'],['.']]).
