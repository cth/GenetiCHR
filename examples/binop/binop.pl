% A tiny interpreter for boolean expressions

and(1,1,1).
and(1,0,0).
and(0,1,0).
and(0,0,0).

bnot(1,0).
bnot(0,1).

or(0,0,0).
or(1,0,1).
or(0,1,1).
or(1,1,1).

xor(0,0,0).
xor(1,0,1).
xor(0,1,1).
xor(1,1,0).

binop(Variables,[BinaryPred, X, Y], Out) :-
	binop(Variables,X,XVal),
	binop(Variables,Y,YVal),
	Goal =.. [ BinaryPred, XVal, YVal, Out],
	call(Goal).	
	
binop(Variables,[not, X], Out) :-
	binop(Variables,X,XVal),
	bnot(XVal,Out).

binop(Variables, X, Out) :-
	atom(X),
	variable_lookup(Variables,X,Out).

binop(1,1).
binop(0,0).

variable_lookup([[X,Value]|_],X,Value).
variable_lookup([[Y,_]|Rest],X,Value) :-
	X \= Y,
	variable_lookup(Rest,X,Value).

sort_expr(0,0).
sort_expr(1,1).
sort_expr(A,A) :- atom(A), !. % variables

sort_expr([Op,A,B],[Op|Sorted]) :-
	atom(A),
	atom(B),
	!,
	sort([A,B],Sorted).
	
sort_expr([Op,A,B],[Op,B,A]) :-
	atom(B).

sort_expr([Op,A,B],SortExpr) :-
	sort_expr(A,Ax),
	sort_expr(B,Bx),
	(sort_expr([Ax,Bx],[Ax,Bx]) ->
		SortExpr = [Op,Ax,Bx]
		;
		SortExpr = [Op,Bx,Ax]).
		
sort_expr([Op,A],[Op,Ax]) :-
	sort_expr(A,Ax).
	
sort_simplify(Expr,SimpExpr) :-
	sort(Expr,SortExpr),
	simplify(SortExpr,SimpExpr).

% Simple truths
simplify([and,0,_],0).
simplify([and,_,0],0).
simplify([and,1,X],X).
simplify([and,X,1],X).
simplify([xor,A,A],0).
simplify([or,A,A],1).
simplify([or,1,_],1).
simplify([or,_,1],1).
simplify([not,1],0).
simplify([not,0],1).

% De-Morgans laws
simplify([and,[not,A],[not,B]], Simplified) :-
	!,
	simplify([not,[or,A,B]],Simplified).

simplify([or,[not,A],[not,B]],Simplified) :-
	!,
	simplify([not,[and,A,B]], Simplified).

% Double negation:
simplify([not,[not,X]],Simplified) :-
	!,
	simplify(X,Simplified).

% And of two identical values is equal to value
simplify([and,A,A], Simplified) :-
	!,
	simplify(A,Simplified).
		
% Simplify either branch
simplify([Op,A,B], Expr) :-
	simplify(A,Ax),
	A \= Ax,
	!,
	simplify([Op,Ax,B],Expr).
	
simplify([Op,A,B], Expr) :-
	simplify(B,Bx),
	B \= Bx,
	!,
	simplify([Op,A,Bx],Expr).

% Every other simplication rule has failed
simplify(X,X).

t1 :- binop([[a,0],[b,1]],[and,[xor,b,a],[xor,a,b]],1).

t2 :- 
	sort_expr([or,[and,[xor,b,a],[xor,a,b]],[and,[xor,b,a],[xor,a,b]]],X),
	simplify(X,Y),
	write(X),nl,
	write(Y),nl.
t3 :- 
	sort_expr([and,[and,[xor,b,a],[xor,a,b]],[and,[xor,b,a],[xor,a,b]]],X),
	write(X),nl.	