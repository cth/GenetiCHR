% Implementation of some common crossover methods


% One point cross over assumes equal length of genomes
% A single crossover point (|) on both parents' organism strings is selected.
% All data beyond that point in either organism string is swapped between the two parent organisms:
%
% Parent1:	xxxxxxx|xxxxxxxxx
% Parent2:  yyyyyyy|yyyyyyyyy
% Child1:   xxxxxxx|yyyyyyyyy
% Child2:   yyyyyyy|xxxxxxxxx
one_point_crossover(MotherGenome,FatherGenome,Child1Genome,Child2Genome) :-
	length(FatherGenome, FLen),
	length(MotherGenome, MLen),
	FLen = MLen,
	random(R),
	CrossoverPoint is round(R*FLen),
	from_start(CrossoverPoint, MotherGenome,MotherPart1),
	append(MotherPart1, MotherPart2, MotherGenome),
	from_start(CrossoverPoint, FatherGenome,FatherPart1),
	append(FatherPart1, FatherPart2, FatherGenome),
	append(FatherPart1, MotherPart2, Child1Genome),
	append(MotherPart1, FatherPart2, Child2Genome).

% Two-point crossover calls for two points to be selected on the parent organism strings. 
% Everything between the two points is swapped between the parent organisms, rendering two child organisms:
% 
% Parent1:	xxxxxxx|xxxxx|xxx
% Parent2:  yyyyyyy|yyyyy|yyy
% Child1:   xxxxxxx|yyyyy|xxx
% Child2:   yyyyyyy|xxxxx|yyy
two_point_crossover(MotherGenome,FatherGenome,Child1Genome,Child2Genome) :-
	one_point_crossover(MotherGenome,FatherGenome,TmpChild1,TmpChild2),
	one_point_crossover(TmpChild1,TmpChild2,Child1Genome,Child2Genome).


% Another crossover variant, the "cut and splice" approach, results in a change in length of the children strings.
% The reason for this difference is that each parent string has a separate choice of crossover point.
% 
% Parent1:	xxx|xxxxxxxxxxxxx
% Parent2:  yyyyyyyyy|yyyyyyy
% Child1:   xxx|yyyyyyyyy
% Child2:   yyyyyyyyy|xxxxxxxxxxxxx
cut_and_splice(MotherGenome,FatherGenome,Child1Genome,Child2Genome) :-
	length(FatherGenome, FLen),
	length(MotherGenome, MLen),	
	random(R1),
	random(R2),
	CrossoverPointFather is round(R1*FLen),
	CrossoverPointMother is round(R2*MLen),
	from_start(CrossoverPointMother, MotherGenome,MotherPart1),
	append(MotherPart1, MotherPart2, MotherGenome),
	from_start(CrossoverPointFather, FatherGenome,FatherPart1),
	append(FatherPart1, FatherPart2, FatherGenome),
	append(FatherPart1, FatherPart2, FatherGenome),
	append(FatherPart1, MotherPart2, Child1Genome),
	append(MotherPart1, FatherPart2, Child2Genome).

% In the uniform crossover scheme (UX) individual bits in the string are compared between two parents.
% The bits are swapped with a fixed probability, typically 0.5.
uniform_crossover([],[],[],[], _).
uniform_crossover([MGene|MRest],[FGene|FRest], [FGene|C1Rest],[MGene|C2Rest], SwapProbability) :-
	random(R),
	R >= SwapProbability,
	!,
	uniform_crossover(MRest,FRest,C1Rest,C2Rest,SwapProbability).
uniform_crossover([MGene|MRest],[FGene|FRest], [MGene|C1Rest],[FGene|C2Rest], SwapProbability) :-
	uniform_crossover(MRest,FRest,C1Rest,C2Rest,SwapProbability).

% Fixme: Add Edge recombination operator

from_start(0,_,[]).
from_start(N,[C|Rest1],[C|Rest2]) :-
	N1 is N - 1,
	from_start(N1,Rest1,Rest2).
	
from_end(N,In,Out) :-
	reverse(In,RevIn),
	from_start(N,RevIn,RevOut),
	reverse(RevOut,Out).