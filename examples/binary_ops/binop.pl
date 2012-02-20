% A tiny intepreter for a lisp like language
% minilisp(+VariableBindings, +Program, -Output)

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
	
binop(Variables,[UnaryPred, X], Out) :-
	binop(Variables,X,XVal),
	Goal =.. [ UnaryPred, XVal, Out],
	call(Goal).

binop(Variables, X, Out) :-
	atom(X),
	variable_lookup(Variables,X,Out).

binop(1,1).
binop(0,0).

variable_lookup([[X,Value]|_],X,Value).
variable_lookup([[Y,_]|Rest],X,Value) :-
	X \= Y,
	variable_lookup(Rest,X,Value).

t1 :-
	binop([[a,0],[b,1]],[and,[xor,b,a],[xor,a,b]],1).

