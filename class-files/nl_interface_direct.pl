% Prolog representation of a grammar to build a query for a database
%  This is not meant to be polished or lingustically reasonable, but purely to show what can be done

% This is slightly expanded code of Figure 13.11 in Section 13.6.6 of
% Poole and Mackworth, Artificial Intelligence: foundations of
% computational agents, Cambridge, 2017

% Copyright (c) David Poole and Alan Mackworth 2017. This program
% is released under GPL, version 3 or later; see http://www.gnu.org/licenses/gpl.html

% noun_phrase(T0,T4,Ind) is true if
%  T0 and T4 are list of words, such that
%        T4 is an ending of T0
%        the words in T0 before T4 (written T0-T4) form a noun phrase
%  Ind is an individual that the noun phrase is referring to

% A noun phrase is a determiner followed by adjectives followed
% by a noun followed by an optional modifying phrase:
noun_phrase(T0,T4,Ind) :-
    det(T0,T1,Ind),
    adjectives(T1,T2,Ind),
    noun(T2,T3,Ind),
    mp(T3,T4,Ind).

% Try:
%?- noun_phrase([a,tall,student],T1,I1).
%?- noun_phrase([a,math,course],T2,I2).
%?- noun_phrase([a,tall,student,enrolled,in,a,math,course],T3,I3).

% Determiners (articles) are ignored in this oversimplified example.
% They do not provide any extra constraints.
det([the | T],T,_).
det([a | T],T,_).
det(T,T,_).

% adjectives(T0,T1,Ind) is true if 
% T0-T1 is an adjective is true of Ind
adjectives(T0,T2,Ind) :-
    adj(T0,T1,Ind),
    adjectives(T1,T2,Ind).
adjectives(T,T,_).

% An optional modifying phrase / relative clause is either
% a relation (verb or preposition) followed by a noun_phrase or
% 'that' followed by a relation then a noun_phrase or
% nothing 
mp(T0,T2,Subject) :-
    reln(T0,T1,Subject,Object),
    noun_phrase(T1,T2,Object).
mp([that|T0],T2,Subject) :-
    reln(T0,T1,Subject,Object),
    noun_phrase(T1,T2,Object).
mp(T,T,_).

% DICTIONARY

% adj(T0,T1,Ind) is true if T0-T1 is an adjective that is true of Ind
adj([computer, science | T],T,Ind) :- dept(Ind,comp_sci).
adj([math | T],T,Ind) :- dept(Ind,math).
adj([female | T],T,Ind) :- female(Ind).
adj([male | T],T,Ind) :- male(Ind).
adj([tall | T],T,Ind) :- tall(Ind).

% noun(T0,T1,Ind) is true if T0-T1 is a noun that is true of Ind
noun([course | T],T,Ind) :- course(Ind).
noun([student | T],T,Ind) :- student(Ind).
noun([building | T],T,Ind) :- building(Ind).
% The following are for proper nouns:
noun([Ind | T],T,Ind) :- course(Ind).
noun([Ind | T],T,Ind) :- student(Ind).


% reln(T0,T1,Subject,Object) is true if T0-T1 is a relation
%   on individuals Subject and Object
reln([enrolled, in | T],T,Subject,Object) :- enrolled_in(Subject,Object).
reln([passed | T],T,Subject,Object) :- passed(Subject,Object).

% Some Example Queries
% ask noun_phrase([a,computer,science,course],R,Ind).
% ask noun_phrase([a,tall,student,enrolled,in,a,computer,science,course],R,Ind).

% question(Question,QR,Ind) is true if Question-QR is true of Ind
question([is | T0],T2,Ind) :-
    noun_phrase(T0,T1,Ind),
    mp(T1,T2,Ind).
question([who,is | T0],T1,Ind) :-
    mp(T0,T1,Ind).
question([who,is | T0],T1,Ind) :-
    noun_phrase(T0,T1,Ind).
question([who,is | T0],T1,Ind) :-
    adjectives(T0,T1,Ind).
question([what | T0],T2,Ind) :-
    noun_phrase(T0,T1,Ind),
    mp(T1,T2,Ind).
% The following has "is" betten the noun_phrase and the mp:
question([what | T0],T2,Ind) :-
    noun_phrase(T0,[is|T1],Ind),
    mp(T1,T2,Ind).


% ask(Q,A) gives answer A to question Q
ask(Q,A) :-
    question(Q,[],A).


%  The Database of Facts to be Queried

% course(C) is true if C is a course
course(cs312).
course(cs322).
course(math315).

dept(cs312,comp_sci).
dept(cs322,comp_sci).
dept(math315,math).

enrolled_in(john,cs312).
enrolled_in(mary,cs312).
enrolled_in(jane,math315).
enrolled_in(sally,cs322).
enrolled_in(sam,math315).

passed(S,C):-
    grade(S,C,G),
    G >= 50.

grade(sam,cs312,93).
grade(chris,cs312,82).

female(mary).
female(jane).
female(sally).
male(john).

tall(mary).
tall(jane).
tall(john).
tall(jordan).

student(mary).
student(jane).
student(sally).
student(john).
student(sam).
student(chris).

/* Try the following queries
| ?- ask([is,mary,enrolled,in,cs312],_).
| ?- ask([who,is,a,student],A).
| ?- ask([who,is,tall],A).
| ?- ask([who,is,a,tall,student],A).
| ?- ask([is,mary,enrolled,in,a,computer,science,course],_).
| ?- ask([who,is,enrolled,in,a,computer,science,course],A).
| ?- ask([who,is,a,tall,student,enrolled,in,a,computer,science,course],A).
| ?- ask([what,student,is,enrolled,in,a,computer,science,course],A).
| ?- ask([what,student,passed,a,computer,science,course],A).
| ?- ask([what,student,enrolled,in,a,math,course,passed,a,computer,science,course],A).
| ?- ask([what,student,passed,a,computer,science,course,enrolled,in,a,math,course],A).
| ?- ask([what,student,passed,cs312],A).
*/

% To get the input from a line:

q(Ans) :-
    write("Ask me: "),flush_output(current_output),
    readln(Ln),
    question(Ln,End,Ans),
    member(End,[[],['?'],['.']]).

/*
?- q(Ans).
Ask me: who is a tall student enrolled in a computer science course?
Ans = mary ;
Ans = john ;
false.
*/
