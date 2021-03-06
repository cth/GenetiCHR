% A simple demonstration program that demonstrates the evolution of a string
% using the genetic algorithm framework.

:- ['../../genetichr']. % Load the genetic algorithm framework
:- ['levenshtein']. % We use Stephan Maiers edit distance implementation for fitness.

target_string("cafebabe"). % Find the cafe babe
characters("abcdef").

run_example :-
	population_size(50), % The number of individuals in each generation
	mutation_rate(0.1),
	crossover_rate(0.2),
	fitness_threshold(0),
	generation_threshold(1000),
	selection_mode(tournament), % Survisors are selected using elitism scheme
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

%cb_create_individual("abba abba").
	
cb_calculate_fitness(Genome,Fitness) :-
	target_string(Target),
	atom_codes(Atom1,Target),
	atom_codes(Atom2,Genome),
	levenshtein(Atom1,Atom2,Distance),
	Fitness is 0 - Distance.

cb_mutate(Genome, MutatedGenome) :-
	characters(C),
	multi_point_mutate(Genome,MutatedGenome,C,0.1).

cb_crossover(GenomeFather,GenomeMother,ChildGenome1,Child2Genome) :-
	cut_and_splice(GenomeFather,GenomeMother,ChildGenome1,Child2Genome).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Various utility rules used by cb_ rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
	CharIndex is round(P * (NumChars-1)),
	nth0(CharIndex,Chars,Char).