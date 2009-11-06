:- [crossover].
:- [mutation].
:- use_module(library(chr)).
:- chr_constraint phase/1, generation/1, individual/4,  mutate/1, cross_over/2, uniq_id/1,
	population_size/1, population_counter/1, collect_generation/1, survivors/2, insert_survivors/0,
	mutation_rate/1, cross_over_rate/1, selection_mode/1,
	fitness_threshold/1, generation_threshold/1, best_individual/1, average_fitness/1,
	report_on_cycle/0.

% Assign unique identifier to a new child
individual(new_id,Generation,Genome,Fitness), uniq_id(Id) <=>
	NextId is Id + 1,
	individual(Id,Generation,Genome,Fitness),
	uniq_id(NextId).

% Fitness calculation: The user is expected to have implemented cb_calculate_fitness/2 which
% must return a number representing the fitness of the individual (higher = more fit).
calculate_fitness @
individual(Id,Generation,Genome,fitness_unknown) <=>
	cb_calculate_fitness(Genome,Fitness),
	individual(Id,Generation,Genome,Fitness).

% Crossover: Certain individuals are selected for cross over. The user is expected to implement
% the cb_cross_over/3 rule, where the third argument must unify with 
cross_over @
generation(G), individual(Id1,_,Genome1,_), individual(Id2,_,Genome2,_) \ cross_over(Id1,Id2) <=>
	cb_cross_over(Genome1,Genome2,ChildGenome1,ChildGenome2),
	NextGeneration is G + 1,
	individual(new_id,NextGeneration,ChildGenome1,fitness_unknown),
	individual(new_id,NextGeneration,ChildGenome2,fitness_unknown).
	
% Mutation: Inviduals are selected for mutation with a probability defined by the mutation rate
% The user is expected to implement the cb_mutate/2 which mutates a genome. 
mutate @
mutate(Id), individual(Id,Generation,Genome,_) <=>
	cb_mutate(Genome,MutatedGenome),
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
	%write('created new individual: '), write(Genome),nl
	|
	NewPopSize is PopSize - 1,
	individual(new_id,0,Genome,fitness_unknown),
	population_counter(NewPopSize).

phase(mutation) ==>	write('  - mutation phase'),nl.

phase(mutation), mutation_rate(Rate), individual(Id,_,_,_) ==>
	random(Number),
	Number =< Rate
	|
%	write('mutating '), write(Id), nl,
	mutate(Id).

phase(mutation) <=> write('  - cross over phase'),nl, phase(cross_over).

generation(G), phase(cross_over), cross_over_rate(Rate), individual(Id1,G1,_,_), individual(Id2,G2,_,_) ==>
	Id1 < Id2,
	%write(G), write(' '), write(G1), write(' '), write(G2), nl,
	G >= G1, G >= G2, % Do not cross-over children from this generation
	random(Number),
	Number =< Rate*Rate
	|
%	write('crossing over '), write(Id1), write(' and '), write(Id2),nl,
	cross_over(Id1,Id2).
	
phase(cross_over) <=> write('  - selection phase'), nl, phase(selection).

% Report best individual and average fitness each evolutionary cycle
individual(Id,Generation,Genome,Fitness) \ report_on_cycle, best_individual(Id), average_fitness(MeanFit) <=>
	write('Best individual: , '), write(individual(Id,Generation,Genome,Fitness)), nl,
	write('Average fitness of generation: '), write(MeanFit), nl.

% Termination because we found a good enough individual
best_individual(Id), individual(Id,_,_,Fitness) \ generation(_), phase(evolution_cycle_done), fitness_threshold(Threshold) <=>
	Fitness >= Threshold
	|
	write('Individual with fitness threshold '),write(Threshold),write(' found. Terminating.'),nl,
	report_on_cycle.

% Termination because with reached the maximal generation threshold
generation(Threshold), phase(evolution_cycle_done), generation_threshold(Threshold) <=>
	write('Reached generation threshold'), write(Threshold),	write('. Terminating.'),nl,
	report_on_cycle.

% Evolutionary cycle done - proceeed to next cycle.	
generation(X) \ phase(evolution_cycle_done) <=> 
	Y is X + 1,
	write('Evolution cycle done. Starting on generation '),write(Y),nl,
%	chr_show_store(''),
	report_on_cycle,
%	sleep(1), 	
	generation(Y),
	phase(mutation).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utilities used by multiple selection schemes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

% Collecting all live individuals in current generation
collect_generation(L), individual(Id,Generation,Genome,Fitness) <=>
	collect_generation([individual(Id,Generation,Genome,Fitness)|L]).
	
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

insert_survivors, survivors(0, _) <=> phase(evolution_cycle_done).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Elitism selection:
% Select the best individuals from evolved generation and kill the rest
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

phase(selection), selection_mode(elitism) ==> collect_generation([]).

phase(selection), selection_mode(elitism), population_size(PopSize) \ collect_generation(Generation) <=>
	fitness_sort(Generation,Sorted),
	Sorted = [individual(BestId,_,_,_)|_],
	sum_fitness(Sorted,TotalFitness),
	length(Sorted, TotalIndividuals),
	MeanFit is TotalFitness / TotalIndividuals,
	average_fitness(MeanFit),
	best_individual(BestId),
	survivors(PopSize, Sorted),
	insert_survivors.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tournament selection 
% A tournament is not guaranteed to have exactly tournament_size 
% competitors, but it will on average. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- chr_constraint tournaments/1, tournament/2, start_tournaments/0, tournament_size/1.

phase(selection), selection_mode(tournament), tournament_size(TournamentSize) \ collect_generation(Generation) <=>
	length(Generation,GenerationSize),
	NumTournaments is GenerationSize / TournamentSize,
	tournaments(NumTournaments).

tournament(Id, Competitors1), tournament(Id,Competitors2) <=>
	append(Competitors1, Competitors2, Competitors),
	tournament(Id,Competitors).

% Assign each individual to a random tournament
tournaments(NumTournaments) \ individual(Id,Generation,Genome,Fitness) <=>
	random(R),
	AssignedTournament is round(R*NumTournaments),
	tournament(AssignedTournament,[individual(Id,Generation,Genome,Fitness)]).

% When all individuals have been assigned a tournament, start the tournaments
tournaments(_) <=> start_tournaments.

start_tournaments, tournament(_,Competitors) <=>
	fitness_sort(Competitors,[Winner|_]),
	survivors(1,[Winner]).

start_tournaments <=> insert_survivors.

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


fitness_sort(List,Sorted):-q_sort(List,[],Sorted).
q_sort([],Acc,Acc).
q_sort([H|T],Acc,Sorted):-
	pivoting(H,T,L1,L2),
	q_sort(L1,Acc,Sorted1),q_sort(L2,[H|Sorted1],Sorted).

pivoting(_,[],[],[]).
pivoting(H,[X|T],[X|L],G) :- H=individual(_,_,_,FitH), X=individual(_,_,_,FitX), FitX=<FitH, pivoting(H,T,L,G).
pivoting(H,[X|T],L,[X|G]):- H=individual(_,_,_,FitH), X=individual(_,_,_,FitX), FitX>FitH, pivoting(H,T,L,G).