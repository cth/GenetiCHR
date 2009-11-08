% Implementation of some common crossover methods
% Part of the GenetiCHR library
% (c) Christian Theil Have, 2009.

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
	
% Binary tree cross-over: Swaps two random nodes in the two binary trees:
% Parent1: [mul,2,[plus,a,b]]
% Parent2: [plus,[mul,a,b],a]
% Child1: T1 = [mul, 2, b],
% Child2: T2 = [plus, [mul, a, [plus, a, b]], a]
binary_tree_crossover(Tree1,Tree2,Tree3,Tree4) :-
	count_nodes(Tree1,TreeNodeCount1),
	count_nodes(Tree2,TreeNodeCount2),
%	write('count nodes: '), write(TreeNodeCount1),write(' '), write(TreeNodeCount2),nl,
	random(R1),
	random(R2),
	SelectNodeId1 is round(R1*(TreeNodeCount1-1)),
	SelectNodeId2 is round(R2*(TreeNodeCount2-1)),
%crossover	write('selected nodes: '), write(SelectNodeId1),write(' '), write(SelectNodeId2),nl,
	extract_node(Tree1,SelectNodeId1,0,SelectNode1),
	extract_node(Tree2,SelectNodeId2,0,SelectNode2),	
	exchange_node(Tree1,SelectNode2,SelectNodeId1,0,Tree3),
	exchange_node(Tree2,SelectNode1,SelectNodeId2,0,Tree4).
	
count_nodes(X,1) :- atom(X) ; number(X).
count_nodes([_,Rest],Nodes) :-
	count_nodes(Rest,RestNodes),
	Nodes is 1 + RestNodes.
count_nodes([_,Left,Right],Nodes) :-
	count_nodes(Left,LeftNodes),
	count_nodes(Right,RightNodes),
	Nodes is 1 + LeftNodes + RightNodes.

% exchange_node/5 replaces a node in a tree with some other other Tree (ReplacementNode)

% Target node:
exchange_node(_,ReplacementNode,TargetNodeId,TargetNodeId,ReplacementNode).

% Unary nodes:
exchange_node([Op,Rest],ReplacementNode,TargetNodeId,CurrentNodeId,[Op,RestReplace]) :-
	TargetNodeId \= CurrentNodeId,
	NextNodeId is CurrentNodeId + 1,	
	exchange_node(Rest,ReplacementNode,TargetNodeId,NextNodeId,RestReplace).

% Binary nodes:
exchange_node([Op,Left,Right],ReplacementNode,TargetNodeId,CurrentNodeId,[Op,LeftReplace,RightReplace]) :-
	TargetNodeId \= CurrentNodeId,
	count_nodes(Left,LeftNodes),
	LargestLeft is LeftNodes + CurrentNodeId,
	((LargestLeft >= TargetNodeId) ->
		NextNodeId is CurrentNodeId + 1,
		exchange_node(Left,ReplacementNode,TargetNodeId,NextNodeId,LeftReplace),
		RightReplace = Right
		;
		NextNodeId is LargestLeft + 1,
		exchange_node(Right,ReplacementNode,TargetNodeId,NextNodeId,RightReplace),
		LeftReplace = Left
	).

% Target node:	
extract_node(X,TargetNodeId,TargetNodeId,X).

% Unary nodes:
extract_node([_,Rest],TargetNodeId,CurrentNodeId,TargetNode) :-
	TargetNodeId \= CurrentNodeId,
	NextNodeId is CurrentNodeId + 1,
	extract_node(Rest,TargetNodeId,NextNodeId,TargetNode).

% Binary nodes:
extract_node([_,Left,Right],TargetNodeId,CurrentNodeId,TargetNode) :-
	TargetNodeId \= CurrentNodeId,
	count_nodes(Left,LeftNodes),
	LargestLeft is LeftNodes + CurrentNodeId,
	((LargestLeft >= TargetNodeId) ->
		NextNodeId is CurrentNodeId + 1,
		extract_node(Left,TargetNodeId,NextNodeId,TargetNode)
		;
		NextNodeId is LargestLeft + 1,
		extract_node(Right,TargetNodeId,NextNodeId,TargetNode)
	).

% Fixme: Add Edge recombination operator

from_start(0,_,[]).
from_start(N,[C|Rest1],[C|Rest2]) :-
	N1 is N - 1,
	from_start(N1,Rest1,Rest2).
	
from_end(N,In,Out) :-
	reverse(In,RevIn),
	from_start(N,RevIn,RevOut),
	reverse(RevOut,Out).