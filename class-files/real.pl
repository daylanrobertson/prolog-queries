:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_db)).


% Load schema.org ontology
:- rdf_load('http://schema.org/version/latest/all-layers.rdf').





%? -rdf(S, P, O).

%        Subject: Justin Bieber                  Property: name           Object: name value
%?- rdf('http://www.wikidata.org/entity/Q34086','http://schema.org/name',literal(lang(en,N))).


%?- rdf('http://www.wikidata.org/entity/Q34086', 'http://www.wikidata.org/prop/direct/P25',M).  

%?- rdf(E, 'http://schema.org/name', O).



%?- rdf('http://www.wikidata.org/entity/Q34086','http://schema.org/name',literal(lang(en,N))).



% rdf('http://www.wikidata.org/entity/Q34086','http://schema.org/name',literal(lang(en,N))).1
%?- rdf('http://www.wikidata.org/entity/Q34086', 'http://www.wikidata.org/prop/direct/P25',M).  % mother
%?- rdf('http://www.wikidata.org/entity/Q2368906', 'http://schema.org/name', literal(lang(en,N))).




% find the name id of a name.
%? - rdf(S, 'http://schema.org/name', literal(lang(en,'Selena Gomez'))).

mother(P, N) :- 
        rdf(S, 'http://schema.org/name', literal(lang(en,P))),
        rdf(S, 'http://www.wikidata.org/prop/direct/P25',M),
        rdf(M,'http://schema.org/name',literal(lang(en,N))).



%:- rdf_load('http://www.wikidata.org/wiki/Special:EntityData/Q83287.rdf').   % Gretzky
% find the name id of a name.
%? - rdf(S, 'http://schema.org/name', literal(lang(en,'Selena Gomez'))).
% Selena Gomez
% rdf('http://www.wikidata.org/entity/Q83287','http://schema.org/name', literal(lang(en,N))).





%:- rdf_load('http://www.wikidata.org/wiki/Special:EntityData/Q209518.rdf').   % Gretzky
% rdf('http://www.wikidata.org/entity/Q209518','http://schema.org/name', literal(lang(en,N))).
% rdf(S, 'http://schema.org/name', literal(lang(en,'Wayne Gretzky'))).







