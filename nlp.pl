consult(wn_s).

% (noun) is/are (adjectives)
sentence(T0,T4,Ind):-
	det(T0,T1,Ind),
	check_noun(T1,T2,Ind),
	check_is_are(T2,T3),
	adj(T3,T4,Ind).


question([A,Ind,Adj|T],T,Ind):-
	is_auxiliary_verb(A),
	prop(Ind,is,Adj).
/*
question([is,Ind,T0],T1,Ind):-
	noun_phrase(T0,T1,Ind).*/%does not work for now
question([who,A | T0],T1,Ind) :-
	is_auxiliary_verb(A),
    noun_phrase(T0,T1,Ind).
question([who,A | T0],T1,Ind) :-
 	is_auxiliary_verb(A),
   	adjectives(T0,T1,Ind).
question([what,A | T0],T1,Ind) :-
	is_auxiliary_verb(A),
    noun_phrase(T0,T1,Ind).
question([what,A | T0],T1,Ind) :-
	is_auxiliary_verb(A),
    adjectives(T0,T1,Ind).

noun_phrase(T0,T4,Ind) :-
    det(T0,T1,Ind),
    adjectives(T1,T2,Ind),
    noun(T2,T3,Ind),
    mp(T3,T4,Ind).

noun_phrase(T0,T4,Ind) :-
    det(T0,T1,Ind),
    check_noun(T1,T2,N1),
    check_preposition(T2,T3),
    check_noun(T3,T4,N2),
    prop(N2,N1,Ind).



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

check_is_are([A|T],T):-
	is_auxiliary_verb(A).

check_preposition([P|T],T):-
	is_preposition(P).

mp(T,T,_).


check_noun([N|T],T,N):-
	is_noun(N).

is_noun(N):-
	prop(_,subclass,N).
is_noun(N):-
	prop(N,_,_).
is_noun(N):-
	s(_,_,N,n,_,_).

is_adj(A):-
	s(_,_,A,a,_,_).

is_preposition(P):-
	member(P,[of,to,in,on]).

is_auxiliary_verb(V):-
	member(V,[am,is,am,are,were]).
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

prop(jordan,height,190).

prop(john,is,smart).
prop(sally,is,beautiful).

prop('godfather', is, good).

prop(animal, subclass,elephant).
prop(elephant,is,large).

prop(student,subclass,mary).
prop(student,subclass,jane).
prop(student,subclass,sally).
prop(student,subclass,john).
prop(student,subclass,sam).
prop(student,subclass,chris).

prop(movie,subclass,'godfather').
prop(movie,subclass,'the dark knight').


% jordan is a teacher of cs312
prop(cs312,teacher,jordan).
prop('godfather',director,'Francis Ford Coppola').
prop('godfather',actor,'Marlon Brando').
prop('the dark knight', director, 'Christopher Nolan').


prop(N1,subclass,N2):-
	prop(_,N1,N2),
	s(_,_,N1,n,_,_).


prop(S,pass,C):-
	grade(S,C,G),
    G >= 50.

prop(person,subclass,A):-
	prop(student,subclass,A).

prop(object,subclass,A):-
	prop(course,subclass,A).
prop(object,subclass,A):-
	prop(animal,subclass,A).
%prop(A,subclass,B):-


/*
prop(A,subclass,C):-
	prop(A,subclass,B),
	prop(B,subclass,C).
*/

grade(sam,cs312,93).
grade(chris,cs312,82).


form_sentence_from_question([_,is|T],Ind,Ans):-
	atomic_list_concat([i,think,Ind,is|T], ' ', Ans).
form_sentence_from_question(_,Ind,Ind).


startNLP:-
    write("what do you want to ask?"),nl,flush_output(current_output),
    readln(Ln),
    distinct(process(Ln,Ans)),
    write(Ans),nl,
    startNLP.

syntaxIO(Type):-
	write("what do you want to ask?"),nl,flush_output(current_output),
    readln(Ln),
    checkSyntax(Ln,Type).

%does not work for now
checkSyntax(Ln,question):-
	check_is_are(Ln,T0),
	checkNoun(T0,T1),
	checkAdj(T1,End),
	End = '?'.

remove_last([_], []).
remove_last([X|Xs], [X|WithoutLast]) :- 
    without_last(Xs, WithoutLast).

%if question is true about somthing
process(Ln,Ind):-
    question(Ln,End,Ind),
    member(End,[[],['?'],['.']]).
    %form_sentence_from_question(Ln,Ind,Ans).

/*
process(Ln,Ans):-
    sentence(Ln,End,Ans),
    member(End,[[],['?'],['.']]).
*/

