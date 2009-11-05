:- use_module(library(chr)).
:- chr_constraint phase/1, generation/1, individual/3,  mutate/1, cross_over/2, uniq_id/1,
	population_size/1, population_counter/1, collect_generation/1, sorted_survivors/2,
	mutation_rate/1, cross_over_rate/1, selection_mode/1.

% Assign unique identifier to a new child
individual(newly_born,Genome,Fitness) \ uniq_id(Id) <=>
	NextId is Id + 1,
	uniq_id(NextId),
	individual(Id,Genome,Fitness).

% Fitness calculation: The user is expected to have implemented cb_calculate_fitness/2 which 
% must return a number representing the fitness of the individual (higher = more fit).
calculate_fitness @
individual(Id,Genome,fitness_unknown) <=> 
	cb_calculate_fitness(Genome,Fitness),
	individual(Id,Genome,Fitness).

% Crossover: Certain individuals are selected for cross over. The user is expected to implement
% the cb_cross_over/3 rule, where the third argument must unify with 
cross_over @
individual(Id1,Genome1,_), individual(Id2, Genome2,_) \ cross_over(Id1,Id2) <=>
	cb_cross_over(Genome1,Genome2,ChildGenome)
	|
	individual(new_id,ChildGenome,fitness_unknown).
	
% Mutation: Inviduals are selected for mutation with a probability defined by the mutation rate
% The user is expected to implement the cb_mutate/2 which mutates a genome. 
mutate @
mutate(Id), individual(Id,Genome,_) <=>
	cb_mutate(Genome,MutatedGenome)
	|
	individual(Id,MutatedGenome,fitness_unknown).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Phase control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

phase(initialization), population_size(S) ==> generation(0), uniq_id(1), population_counter(S).

% When all initial individuals have been created, then start mutatation phase
phase(initialization), population_counter(0), generation(X) <=> 
	write('Generation '), write(X),nl,
	phase(mutation).

phase(initialization) \ population_counter(PopSize) <=>
	cb_create_individual(Genome),
	write('created new individual: '), write(Genome),nl
	|
	NewPopSize is PopSize - 1,
	population_size(NewPopSize),
	individual(newly_born,Genome,fitness_unknown).
	
phase(mutation) ==>	write('  - mutation phase'),nl.

phase(mutation), mutation_rate(Rate), individual(Id,_,_) ==>
	random(Number),
	Number >= Rate
	|
	mutate(Id).

phase(mutation) <=> write('  - cross over phase'), nl, phase(cross_over).

phase(cross_over), cross_over_rate(Rate), individual(Id1,_,_), individual(Id2,_,_) ==>
	random(Number),
	Number >= Rate
	|
	cross_over(Id1,Id2).
	
phase(cross_over) <=> write('  - selection phase'), nl, phase(selection).

generation(X), phase(evolution_cycle_done) <=> 
	Y is X + 1,
	write('Evolution cycle done. Staring on generation '),write(Y),nl,sleep(1), 
	chr_show_store(''),
	generation(Y),
	phase(mutation).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Elitism selection:
% Select the best individuals from evolved generation and kill the rest
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO: Add best and average fitness for each generation

phase(selection), selection_mode(elitism) ==> collect_generation([]).

collect_generation(L), individual(Id,Genome,Fitness) <=>
	collect_generation([individual(Id,Genome,Fitness)|L]).

phase(selection), selection_mode(elitism), population_size(PopSize) \ collect_generation(Generation) <=>
	fitness_sort(Generation,Sorted),
	sorted_survivors(PopSize, Sorted).

sorted_survivors(NumLeft,[Indvidual|Rest]) <=> 
	NumLeft > 0
	|
	NextNumLeft is NumLeft - 1,
	Indvidual,
	sorted_survivors(NextNumLeft,Rest).

sorted_survivors(0, _) <=> phase(evolution_cycle_done).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% End of evolutionary cycle
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

phase(evolution_cycle_done), generation(X) <=>
	Y is X + 1,
	generation(Y),
	phase(mutation).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utility functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fitness_sort(List,Sorted):-q_sort(List,[],Sorted).
q_sort([],Acc,Acc).
q_sort([H|T],Acc,Sorted):-
	pivoting(H,T,L1,L2),
	q_sort(L1,Acc,Sorted1),q_sort(L2,[H|Sorted1],Sorted).

pivoting(_,[],[],[]).
pivoting(H,[X|T],[X|L],G) :- H=individual(_,_,FitH), X=individual(_,_,FitX), FitX=<FitH, pivoting(H,T,L,G).
pivoting(H,[X|T],L,[X|G]):- H=individual(_,_,FitH), X=individual(_,_,FitX), FitX>FitH, pivoting(H,T,L,G).