% CPSC 312 - Some queries on schema.org

:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_db)).

% Load schema.org ontology
:- rdf_load('http://schema.org/version/latest/all-layers.rdf').
% Or first download the file and call
%:- rdf_load('all-layers.nt').

% Try:
%?- rdf(S,P,O).
%?- rdf(S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',P).
%?- rdf(S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/1999/02/22-rdf-syntax-ns#Property').

%?- rdf(S,'http://www.w3.org/2000/01/rdf-schema#subClassOf',P).

% inferred_type(I,C) means I can be inferred to be of type C
inferred_type(I,C) :-
    rdf(I,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',C1),
    subclass(C1,C).

%?- inferred_type(I,C).

% subclass(C1,C2) means C1 is a subclass of C2 (taking transitivity into account)
subclass(C1,C2) :-
    rdf(C1,'http://www.w3.org/2000/01/rdf-schema#subClassOf',C2).
subclass(C1,C3) :-
    rdf(C1,'http://www.w3.org/2000/01/rdf-schema#subClassOf',C2),
    subclass(C2,C3).

%?- subclass(C1,C2), subclass(C2,C3).

% whatis(T,D) is true if D is an English description of T
whatis(T,D) :-
    rdf(T,'http://www.w3.org/2000/01/rdf-schema#comment',D).

% Schema.org does not use domain and range! Try:

% type_from_domain(I,T) means I has type T can be derived from the domain declatation
type_from_domain(I,T) :-
    rdf(I,P,_),
    rdf(P,'http://www.w3.org/2000/01/rdf-schema#domain',T).

% type_from_range(I,T) means I has type T can be derived from the range declatation
type_from_range(I,T) :-
    rdf(_,P,I),
    rdf(P,'http://www.w3.org/2000/01/rdf-schema#range',T).


