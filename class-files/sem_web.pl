% CPSC 312 - examples of interacting with wikidata
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_db)).

:- rdf_load('http://schema.org/version/latest/all-layers.rdf').

%:- rdf_load('https://www.w3.org/People/Berners-Lee/card.rdf').
%:- rdf_load('http://cs.ubc.ca/~poole/foaf.rdf').
%:- rdf_load('http://www.wikidata.org/wiki/Special:EntityData/Q24639.rdf').  % Vancouver
%:- rdf_load('http://www.wikidata.org/wiki/Special:EntityData/Q3099714.rdf'). % Justin Trudeau
%:- rdf_load('http://www.wikidata.org/wiki/Special:EntityData/Q34086.rdf').   % Justin Bieber
%:- rdf_load('http://www.wikidata.org/wiki/Special:EntityData/P25.rdf').   % mother property

%?- rdf(S,P,O).
%?- rdf(S,'http://xmlns.com/foaf/0.1/member',O).
%?- rdf('https://www.w3.org/People/Berners-Lee/card#i',P,O).
%?- rdf('https://www.w3.org/People/Berners-Lee/card#i','http://xmlns.com/foaf/0.1/family_name',O).
%?- rdf('http://www.wikidata.org/entity/Q34086','http://schema.org/name',N).
%?- rdf('http://www.wikidata.org/entity/Q34086','http://schema.org/name',literal(lang(ko,N))).  % Korean
%?- rdf('http://www.wikidata.org/entity/Q34086','http://schema.org/name',literal(lang(az,N))).  % Azerbaijani
%?- rdf('http://www.wikidata.org/entity/Q34086', 'http://www.wikidata.org/prop/direct/P25',M).  % mother
%?- rdf('http://www.wikidata.org/entity/P25',P,V).
   

