:- ['../../genetichr.pl'].
:- [minilisp].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This is the program we are search for:
pythagoras_in_minilisp(A,B,C) :-
	minilisp([[a,A],[b,B]],[sqrt,[plus, [mul,b,b],[mul,a,a]]],C).

% Define the alphabet of the "genome"
symbols([a,b]).
operator(binary, mul).
operator(binary, div).
operator(binary, plus).
operator(binary, minus).
operator(unary, sqrt).

operator_mutation_probability(0.1).
symbol_mutation_probability(0.1).

% Limiting the size of the program may be an advantage (we dont want to consider huge programs)
max_program_size(16).

% We use these example to evaluate a function learned by the algorithm
target_function_examples([
	[3, 4, 5.0],
	[2, 2, 2.82843],
	[1, 1, 1.41421],
	[5, 4, 6.40312],
	[9, 9, 12.7279],
	[10, 10, 14.1421],
	[2, 3, 3.60555],
	[1, 1, 1.41421],
	[7, 2, 7.28011]
]).

% Setup and run the xample
run_example :-
	population_size(200), % The number of individuals in each generation
	mutation_rate(0.5),  % How many individuals are selected for mutation each generation
	crossover_rate(0.9), % How many individuals are selected for crossover each generation
	fitness_threshold(-0.1), % Stop when fitness of the program climbs to -0.1 (0 means perfect program)
	generation_threshold(2000), % Alternatively, stop when we reach the 2000 generation
	selection_mode(tournament), % Survisors are selected using tournament scheme
	phase(initialization). % Start the algorithm

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of callback rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This rule is called every time a new individual should be created 
% for inclusion in the initial generation. It will be invoked as many 
% times as specified by population_size
cb_create_individual(Genome) :-	random_tree(Genome).

% Mutation: Each operator and symbol is mutated to a random
% other operator/symbol with a certain probability
cb_mutate(Symbol,NewSymbol) :-
	atom(Symbol),
	symbol_mutation_probability(MutateProb),
	random(R),
	((R >= MutateProb) -> random_symbol(NewSymbol) ; NewSymbol = Symbol).

cb_mutate([Op,Rest],[NewOp,NewRest]) :-
	operator_mutation_probability(MutateProb),
	random(R),
	((R >= MutateProb) -> random_operator(unary,NewOp) ; NewOp = Op),
	cb_mutate(Rest,NewRest).

cb_mutate([Op,Left,Right], [NewOp,NewLeft,NewRight]) :-
	operator_mutation_probability(MutateProb),
	random(R),
	((R >= MutateProb) -> random_operator(binary,NewOp) ; NewOp = Op),
	cb_mutate(Left,NewLeft),
	cb_mutate(Right,NewRight).
	
% Cross-over: using the binary tree crossover method
cb_crossover(Parent1,Parent2,Child1,Child2) :-
	binary_tree_crossover(Parent1,Parent2,Child1,Child2).

% Fitness function

% Penalize very long programs
cb_calculate_fitness(Program,Fitness) :-
	max_program_size(MaxSize),
	count_nodes(Program,ProgramSize),
	ProgramSize > MaxSize,
	Fitness is 0-99999999,
	!.

% Otherwise, fitness is calculated as the total error 
% on all the examples
cb_calculate_fitness(Program,Fitness) :-
	target_function_examples(Examples),
	catch(run_examples(Program, Examples, Results),_,Error=yes),
	((Error==yes) ->
		Fitness is -99999999
		;
		sum_total_error(Results,Error),
		Fitness is 0 - Error
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Various utility rules used by cb_ rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sum_total_error([],0).
sum_total_error([[ExpectedResult,ActualResult]|Rest], TotalError) :-
	sum_total_error(Rest,TotalErrorRest),
	abs((ExpectedResult-ActualResult),ExampleError),
	TotalError is TotalErrorRest + ExampleError.
	
run_examples(_,[],[]).
run_examples(Program,[[A,B,ExpectedResult]|RestIn],[[ExpectedResult,ActualResult]|RestOut]) :-
	minilisp([[a,A],[b,B]],Program, ActualResult),
	run_examples(Program,RestIn,RestOut).

random_operator(Cardinality, Operator) :-
		findall(Op,operator(Cardinality,Op), Ops),
		length(Ops,OpsLen),
		random(R1),
		OpIdx is round(R1*(OpsLen-1)),
		nth0(OpIdx, Ops, Operator).

random_symbol(Sym) :-
		symbols(Syms),
		length(Syms,SymsLen),
		random(R2),
		SymIdx is round(R2*(SymsLen-1)),
		nth0(SymIdx,Syms,Sym).
		
% Creates random tree for use in initial generation
random_tree(Tree) :- random_tree(0,Tree).
random_tree(Depth, Tree) :-
	random(R),
	BranchingProbability is 1 - 0.2*Depth,
	NewDepth is Depth + 1,
	((R =< BranchingProbability) ->
		random_operator(_,Op),
		random_tree(NewDepth,Left),
		(operator(binary,Op) ->
			random_tree(NewDepth,Right),
			Tree = [ Op, Left, Right ]
			;
			Tree = [ Op, Left]
		)
		;
		random_symbol(Tree)
	).
