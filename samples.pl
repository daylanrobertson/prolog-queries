:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_db)).


% Load schema.org ontology
:- rdf_load('http://schema.org/version/latest/all-layers.rdf').


% Tom Cruise
:- rdf_load('http://www.wikidata.org/wiki/Special:EntityData/Q37079.rdf').

% Justin Bieber
:- rdf_load('http://www.wikidata.org/wiki/Special:EntityData/Q34086.rdf').

:- rdf_load('http://www.wikidata.org/wiki/Special:EntityData/Q163872.rdf').


% Finds all objects that match the pred 'Part Of' for a given name
% Try partOf('Tom Cruise', O).
partOf(N,O) :-
    id(N,I),
    rdf(I,'http://schema.org/isPartOf',O).

% Find the different IDs for an entity given the full name.
% Try id('Tom Cruise', I).
id(N,I) :-
    rdf(I, 'http://schema.org/name', literal(lang(en,N))).


properties(N, P, O) :-
    id(N, I),
    rdf(I, P, O).

% Sample query to find the mother of P, where P is full name.
% Try mother('Justin Bieber', M).
mother(P, N) :- 
    rdf(S, 'http://schema.org/name', literal(lang(en,P))),
    translation(mother, X),
    rdf(S, X, M),
    rdf(M,'http://schema.org/name',literal(lang(en,N))).

% Try sibling('Justin Bieber', M).
sibling(P, N) :- 
    rdf(S, 'http://schema.org/name', literal(lang(en,P))),
    translation(sibling, X),
    rdf(S, X, M),
    rdf(M,'http://schema.org/name',literal(lang(en,N))).

check(N, P, O) :-
    id(N, I),
    translation(P, X),
    rdf(I, X, O).


% Specific function to grab names of actors from a movie. 
% Try cast_list('The Dark Knight', CastMember).
cast_list(M, C) :-
    id(M,I),
    translation(cast, X),
    rdf(I, X, I2),
    id(C, I2).





% some example translations
translation(actor, 'https://schema.org/actor').
translation(mother, 'http://www.wikidata.org/prop/direct/P25').
translation(sibling, 'http://www.wikidata.org/prop/direct/P3373').
translation(title, 'http://schema.org/title').
translation(about, 'http://schema.org/about').

% Movie Specific Properties
translation(pub_date, 'http://www.wikidata.org/prop/direct/P577').
translation(director, 'http://www.wikidata.org/prop/direct/P57').
translation(cast, 'http://www.wikidata.org/prop/direct/P161').
translation(role, 'http://www.wikidata.org/prop/direct/P453').

