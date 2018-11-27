[library(http/dcg_basics),
    library(dialect/sicstus)
   ].

startNLP:-
	write('Hi there, how can I help you?'),nl,
	read(Input),nl,
	atom_string(Input,String),
	process(String,Output),
	write(Output).

process(X, Output):-
	
	Output = X.


