% A simple demonstration program that demonstrates the evolution of a string
% using the genetic algorithm framework.

:- ['../../genetichr']. % Load the genetic algorithm framework

:- ['levenshtein']. % We use Stephan Maiers edit distance implementation for fitness.

target_string("abcdef").
%characters("abcdefghijklmnopqrstuvwxyz ").
characters("abcdef").
point_mutation_probability(0.1).

run_example :-
	population_size(50), % The number of individuals in each generation
	mutation_rate(0.5),
	cross_over_rate(0.2),
	fitness_threshold(0),
	generation_threshold(1000),
	selection_mode(elitism), % Survisors are selected using elitism scheme
	phase(initialization). % Start the algorithm

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of callback rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This rule is called every time a new individual should be created 
% for inclusion in the initial generation. It will be invoked as many 
% times as specified by population_size
cb_create_individual(Genome) :-
	create_random_string(Genome),
	write('cb_create_individual: '), atom_codes(A,Genome), write(A),nl.

%cb_create_individual("netic algerock smogtir").
	
cb_calculate_fitness(Genome,Fitness) :-
	target_string(Target),
	atom_codes(Atom1,Target),
	atom_codes(Atom2,Genome),
	levenshtein(Atom1,Atom2,Distance),
	Fitness is 0 - Distance.

% Creates random point mutation 
cb_mutate([],[]).
cb_mutate([C|GenomeRest], [M|MutatedGenomeRest]) :-
	random(Outcome),
	point_mutation_probability(P),
	((Outcome =< P) -> create_random_character(M) ; M=C),
	cb_mutate(GenomeRest,MutatedGenomeRest).

cb_cross_over(GenomeFather,GenomeMother,ChildGenome) :-
	length(GenomeFather, FLen),
	length(GenomeMother, MLen),
	random(R1),
	random(R2),
	FChars is round(R1*FLen),
	MChars is round(R2*MLen),
	from_start(FChars,GenomeFather,FatherAllele),
	from_end(MChars,GenomeMother,MotherAllele),
	append(FatherAllele,MotherAllele,ChildGenome).
	
t :-
	cb_cross_over([z,x,z,x,z,x,z,x,z,x,z,x,z,x,z,x,z,x,z,x],[a,b,c,d,e,f,g,h,i,j,k,l,m,n],Child),
	atom_codes(Str,Child),
	write(Str),nl,
	t.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Various utility rules used by cb_ rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

from_start(0,_,[]).
from_start(N,[C|Rest1],[C|Rest2]) :-
	N1 is N - 1,
	from_start(N1,Rest1,Rest2).
	
from_end(N,In,Out) :-
	reverse(In,RevIn),
	from_start(N,RevIn,RevOut),
	reverse(RevOut,Out).

% Get the first half of a list
first_half(Str, HalfStr) :-
	length(Str,L),
	HalfFloat is L / 2,
	round(HalfFloat,Half),
	first_half(Half,Str,HalfStr).
	
first_half(0,_,[]) :- !.
first_half(N,[C|StrRest],[C|HalfStrRest]) :-
	N1 is N - 1,
	first_half(N1,StrRest,HalfStrRest).

% Get the last half of a list
last_half(Str,HalfStr) :-
	reverse(Str,RevStr),
	first_half(RevStr,HalfRevStr),
	reverse(HalfRevStr,HalfStr).

%% A simple Markov chain for creating a random string with random length
create_random_string([]) :-
	random(Prob),
	Prob < 0.1, !. % probability of stopping
create_random_string([Char|Rest]) :-
	create_random_character(Char),
	create_random_string(Rest).

create_random_character(Char) :-
	characters(Chars),
	random(P),
	length(Chars,NumChars),
	CharIndexFloat is P * (NumChars-1),
	round(CharIndexFloat, CharIndex),
	nth0(CharIndex,Chars,Char).