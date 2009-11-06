% Some simple mutation function that can be used.

% Single point mutate: Mutate a single element in the list
single_point_mutate(Genome,MutatedGenome,Alphabet) :-
	length(Genome,GLen),
	random(R),
	MutationPoint is round(R*GLen),
	random_mutation(Alphabet,Mutation),
	mutate_position(Genome,MutatedGenome,Mutation,MutationPoint,0).
	
mutate_position([],[], _, _,_).
mutate_position([Gene|Rest1],[Gene|Rest2], Mutation, MutationPosition,PositionCounter) :-
	PositionCounter \= MutationPosition,
	NextPositionCounter is PositionCounter + 1,
	mutate_position(Rest1,Rest2,Mutation,MutationPosition,NextPositionCounter).
mutate_position([_|Rest1],[Mutation|Rest2], Mutation, MutationPosition,PositionCounter) :-
	PositionCounter = MutationPosition,
	NextPositionCounter is PositionCounter + 1,
	mutate_position(Rest1,Rest2,Mutation,MutationPosition,NextPositionCounter).

multi_point_mutate([],[],_,_).
multi_point_mutate([C|GenomeRest], [M|MutatedGenomeRest], Alphabet, PointMutationProbability) :-
	random(Outcome),
	((Outcome =< PointMutationProbability) -> random_mutation(Alphabet,M) ; M=C),
	multi_point_mutate(GenomeRest,MutatedGenomeRest,Alphabet,PointMutationProbability).

% Given an alphabet, this gives you one random entry (Mutation) from the alphabet
random_mutation(Alphabet, Mutation) :-
	random(P),
	length(Alphabet,NumChars),
	CharIndex is round(P * (NumChars-1)),
	nth0(CharIndex,Alphabet,Mutation).