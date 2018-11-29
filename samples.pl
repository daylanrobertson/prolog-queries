:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_db)).


% Load schema.org ontology
:- rdf_load('http://schema.org/version/latest/all-layers.rdf').


% Tom Cruise
:- rdf_load('http://www.wikidata.org/wiki/Special:EntityData/Q37079.rdf').

% Justin Bieber
:- rdf_load('http://www.wikidata.org/wiki/Special:EntityData/Q34086.rdf').

% actor = Q33999
translation(actor, 'https://schema.org/actor').


% Finds all objects that match the pred 'Part Of' for a given name
% Try partOf('Tom Cruise', O).
partOf(N,O) :-
    id(N,I),
    rdf(I,'http://schema.org/isPartOf',O).

% Find the different IDs for an entity given the full name.
% Try id('Tom Cruise', I).
id(N,I) :-
    rdf(I, 'http://schema.org/name', literal(lang(en,N))).


% Sample query to find the mother of P, where P is full name.
% Try mother('Justin Bieber', M).
mother(P, N) :- 
    rdf(S, 'http://schema.org/name', literal(lang(en,P))),
    rdf(S, 'http://www.wikidata.org/prop/direct/P25',M),
    rdf(M,'http://schema.org/name',literal(lang(en,N))).


