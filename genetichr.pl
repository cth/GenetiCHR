:- [crossover].
:- [mutation].
:- use_module(library(chr)).
:- chr_constraint phase/1, generation/1, individual/4,  mutate/1, crossover_list/1, sorted_crossover_list/1, sort_crossover_list/0, crossover/2, uniq_id/1,
	population_size/1, population_counter/1, collect_generation/1, survivors/2, insert_survivors/0,
	mutation_rate/1, crossover_rate/1, selection_mode/1,
	fitness_threshold/1, generation_threshold/1, best_individual/1, total_fitness/1,
	report_on_cycle/0, collect_statistics/0, solution/4.

% Assign unique identifier to a new child
individual(new_id,Generation,Genome,Fitness), uniq_id(Id) <=>
	NextId is Id + 1,
	individual(Id,Generation,Genome,Fitness),
	uniq_id(NextId).

% Fitness calculation: The user is expected to have implemented cb_calculate_fitness/2 which
% must return a number representing the fitness of the individual (higher = more fit).
calculate_fitness @
individual(Id,Generation,Genome,fitness_unknown) <=>
	cb_calculate_fitness(Genome,Fitness), !,
	individual(Id,Generation,Genome,Fitness).

% Crossover: Certain individuals are selected for cross over. The user is expected to implement
% the cb_crossover/3 rule, where the third argument must unify with 
crossover @
generation(G), individual(Id1,_,Genome1,_), individual(Id2,_,Genome2,_) \ crossover(Id1,Id2) <=>
	cb_crossover(Genome1,Genome2,ChildGenome1,ChildGenome2), !,
	NextGeneration is G + 1,
	individual(new_id,NextGeneration,ChildGenome1,fitness_unknown),
	individual(new_id,NextGeneration,ChildGenome2,fitness_unknown).
	
% Mutation: Inviduals are selected for mutation with a probability defined by the mutation rate
% The user is expected to implement the cb_mutate/2 which mutates a genome. 
mutate @
individual(Id,Generation,Genome,_) \ mutate(Id) <=>
	cb_mutate(Genome,MutatedGenome), !,
	individual(Id,Generation,MutatedGenome,fitness_unknown).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Phase control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

phase(initialization), population_size(S) ==> generation(0), uniq_id(1), population_counter(S).

% When all initial individuals have been created, then start mutatation phase
generation(G) \ phase(initialization), population_counter(0) <=> 
	write('Generation '), write(G),nl,
	phase(mutation).

% Create new individuals
phase(initialization) \ population_counter(PopSize) <=>
	cb_create_individual(Genome)
	|
	NewPopSize is PopSize - 1,
	individual(new_id,0,Genome,fitness_unknown),
	population_counter(NewPopSize).

phase(mutation) ==>	write('  - mutation phase'),nl.

phase(mutation), mutation_rate(Rate), individual(Id,_,_,_) ==>
	random(Number),
	Number =< Rate
	|
	mutate(Id).

phase(mutation) <=> write('  - cross over phase'),nl, phase(crossover).

/*
generation(G), phase(crossover), crossover_rate(Rate), individual(Id1,G1,_,_), individual(Id2,G2,_,_) ==>
	Id1 < Id2,
	G >= G1, G >= G2, % Do not cross-over children from this generation
	random(Number),
	Number =< Rate*Rate
	|
	crossover(Id1,Id2).
*/

crossover_list([MatingOrder,Id]), crossover_list(L) <=> crossover_list([[MatingOrder,Id]|L]).

generation(G), phase(crossover), crossover_rate(Rate) ==> crossover_list([]).
generation(G), phase(crossover), crossover_rate(Rate), individual(Id1,G1,_,_) ==>
	G >= G1, % Do not cross-over children from this generation
	random(Number),
	random(MatingOrder),
	Number =< Rate
	|
	crossover_list([MatingOrder,Id1]).

generation(G), phase(crossover), crossover_rate(Rate) ==> sort_crossover_list.

sort_crossover_list, crossover_list(MatingList) <=> length(MatingList,MLL), MLL > 1 | quicksort(MatingList,Sorted),sorted_crossover_list(Sorted).

sorted_crossover_list([]) <=> true. 
sorted_crossover_list([[_,_]]) <=> true. % If only one is selected, crossover not possible.
sorted_crossover_list([[_,Id1],[_,Id2]|Rest]) <=> crossover(Id1,Id2), sorted_crossover_list(Rest).

phase(crossover) <=> write('  - selection phase'), nl, phase(selection).

total_fitness(A), total_fitness(B) <=> C is A + B, total_fitness(C).

best_individual(individual(_,_,_,Fitness1)) \ best_individual(individual(_,_,_,Fitness2)) <=> Fitness1 >= Fitness2 | true.

collect_statistics, individual(Id,Generation,Genome,Fitness) ==>
	total_fitness(Fitness),
	best_individual(individual(Id,Generation,Genome,Fitness)).
	
	
% Report best individual and average fitness each evolutionary cycle
population_size(PopSize) \ collect_statistics, report_on_cycle, best_individual(individual(Id,Generation,Genome,Fitness)), total_fitness(TotalFit) <=>
	MeanFit is TotalFit / PopSize, 
	write('Best individual: '), write(individual(Id,Generation,Genome,Fitness)), nl,
	write('Average fitness of generation: '), write(MeanFit), nl.

% Termination because we found a good enough individual
best_individual(individual(Generation,Unk,Genome,Fitness)) \ generation(_), phase(evolution_cycle_done), fitness_threshold(Threshold) <=>
	Fitness >= Threshold
	|
	write('Individual with fitness threshold '),write(Threshold),write(' found. Terminating.'),nl,
	solution(Generation,Unk,Genome,Fitness),
	report_on_cycle.

% Termination because with reached the maximal generation threshold
generation(Threshold), phase(evolution_cycle_done), generation_threshold(Threshold) <=>
	write('Reached generation threshold'), write(Threshold),	write('. Terminating.'),nl,
	report_on_cycle.

% Evolutionary cycle done - proceeed to next cycle.	
generation(X),  phase(evolution_cycle_done) <=> 
	Y is X + 1,
	write('Evolution cycle done. Starting on generation '),write(Y),nl,
	report_on_cycle,
	generation(Y),
	phase(mutation).
	
get_solution(Generation,Hmm,Genome,Fitness) :-
	find_chr_constraint(solution(Generation,Hmm,Genome,Fitness)).

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utilities used by multiple selection schemes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

% Collecting all live individuals in current generation
collect_generation(L), individual(Id,Generation,Genome,Fitness) <=>
	collect_generation([individual(Id,Generation,Genome,Fitness)|L]).
	
population_size(PopSize) \ survivors(N, List) <=> N > PopSize | survivors(PopSize,List).

survivors(N1,List1), survivors(N2,List2) <=>
	N3 is N1 + N2,
	append(List1,List2,List3),
	survivors(N3,List3).

% Reinsertion of survivoers from generation
insert_survivors \ survivors(NumLeft,[Indvidual|Rest]) <=> 
	NumLeft > 0
	|
	NextNumLeft is NumLeft - 1,
	Indvidual,
	survivors(NextNumLeft,Rest).

phase(selection), insert_survivors, survivors(0, _) <=> 
	collect_statistics,
	phase(evolution_cycle_done).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Elitism selection:
% Select the best individuals from evolved generation and kill the rest
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

phase(selection), selection_mode(elitism) ==> collect_generation([]).

phase(selection), selection_mode(elitism), population_size(PopSize) \ collect_generation(Generation) <=>
	quicksort(Generation,Sorted),
	survivors(PopSize, Sorted),
	insert_survivors.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tournament selection 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- chr_constraint tournament/2, start_tournaments/0, find_tournament_winners/0.

phase(selection), selection_mode(tournament) ==> start_tournaments.

% Competition between two individuals in a particular tournament
tournament(Id, individual(_,_,_,Fitness1)) \ tournament(Id,individual(_,_,_,Fitness2)) <=>	Fitness1 > Fitness2 | true.

% Assign each individual to a random tournament
start_tournaments, phase(selection), selection_mode(tournament), population_size(NumTournaments) \ individual(Id,Generation,Genome,Fitness) <=>
	random(R),
	AssignedTournament is round(R*NumTournaments),
	tournament(AssignedTournament,individual(Id,Generation,Genome,Fitness)).

% When all tournaments are done, find the winners.
start_tournaments <=> find_tournament_winners.

find_tournament_winners \ tournament(_,Winner) <=> survivors(1,[Winner]).

find_tournament_winners <=> insert_survivors.

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

sum_fitness([],0).
sum_fitness([individual(_,_,_,Fit)|Rest],SumFit) :-
	sum_fitness(Rest, SumFitRest),
	SumFit is Fit + SumFitRest.
	
	
qs_compare(H,X,H) :-
	H=individual(_,_,_,FitH),
	X=individual(_,_,_,FitX),
	FitX =< FitH.
	
qs_compare(H,X,X) :-
	H=individual(_,_,_,FitH),
	X=individual(_,_,_,FitX),
	FitX > FitH.
	
qs_compare([R1,Id1],[R2,_],[R1,Id1]) :- R2 =< R1.
qs_compare([R1,_],[R2,Id2],[R2,Id2]) :- R2 > R1.


quicksort(List,Sorted):-q_sort(List,[],Sorted).
q_sort([],Acc,Acc).
q_sort([H|T],Acc,Sorted):-
	pivoting(H,T,L1,L2),
	q_sort(L1,Acc,Sorted1),q_sort(L2,[H|Sorted1],Sorted).

pivoting(_,[],[],[]).
%pivoting(H,[X|T],[X|L],G) :- H=individual(_,_,_,FitH), X=individual(_,_,_,FitX), FitX=<FitH, pivoting(H,T,L,G).
%pivoting(H,[X|T],L,[X|G]):- H=individual(_,_,_,FitH), X=individual(_,_,_,FitX), FitX>FitH, pivoting(H,T,L,G).
pivoting(H,[X|T],[X|L],G) :- qs_compare(H,X,H), pivoting(H,T,L,G).
pivoting(H,[X|T],L,[X|G]):- qs_compare(H,X,X), pivoting(H,T,L,G).

