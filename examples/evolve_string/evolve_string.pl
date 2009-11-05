% A simple demonstration program that demonstrates the evolution of a string
% using the genetic algorithm framework.

:- ['genetic']. % Load the genetic algorithm framework

:- ['levenshtein']. % We use Stephan Maiers edit distance implementation for fitness.

target_string("genetic algorithms rock").
characters("abcdefghijklmnopqrstuvwxyz ").
point_mutation_probability(0.1).

run_example :-
	population_size(20), % The number of individuals in each generation
	mutation_rate(0.1),
	cross_over_rate(0.2),
	selection_mode(elitism), % Survisors are selected using elitism scheme
	phase(initialization). % Start the algorithm

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of callback rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This rule is called every time a new individual should be created 
% for inclusion in the initial generation. It will be invoked as many 
% times as specified by population_size
/*cb_create_individual(Genome) :-
	create_random_string(Genome).
*/
cb_create_individual("netic algerock smogtir").
	
cb_calculate_fitness(Genome,Fitness) :-
	write('calculate fitness'),nl,
	target_string(Target),
	atom_codes(Atom1,Target),
	atom_codes(Atom2,Genome),
	levenshtein(Atom1,Atom2,Fitness).

% Creates random point mutation 
cb_mutate([],[]).
cb_mutate([C|GenomeRest], [M|MutatedGenomeRest]) :-
	random(Outcome),
	point_mutation_probability(P),
	((Outcome =< P) -> create_random_character(M) ; M=C),
	random_mutate(GenomeRest,MutatedGenomeRest).

cb_cross_over(Genome1,Genome2) :-
	first_half(Genome1,FirstHalf),
	last_half(Genome1,LastHalf),
	append(FirstHalf,LastHalf,Genome2).
	
t :- 
	cb_create_individual(G),
	write(G),nl,
	t.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Various utility rules used by cb_ rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
	CharIndexFloat is P * NumChars-1,
	round(CharIndexFloat, CharIndex),
	nth0(CharIndex,Chars,Char).