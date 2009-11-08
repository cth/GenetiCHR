% A tiny intepreter for a lisp like language
% minilisp(+VariableBindings, +Program, -Output)

minilisp(Variables, [mul, X, Y], Out) :-
	minilisp(Variables,X,XVal),
	minilisp(Variables,Y,YVal),
	Out is XVal * YVal.
	
minilisp(Variables, [div, X, Y], Out) :-
	minilisp(Variables,X,XVal),
	minilisp(Variables,Y,YVal),
	Out is XVal / YVal.
	
minilisp(Variables, [plus, X, Y], Out) :-
	minilisp(Variables,X,XVal),
	minilisp(Variables,Y,YVal),
	Out is XVal + YVal.

minilisp(Variables, [minus, X, Y], Out) :-
	minilisp(Variables,X,XVal),
	minilisp(Variables,Y,YVal),
	Out is XVal - YVal.

minilisp(Variables, [sqrt, X], Out) :-
	minilisp(Variables,X,XVal),
	sqrt(XVal,Out).

minilisp(Variables, X, Out) :-
	atom(X),
	variable_lookup(Variables,X,Out).
minilisp(_, X, X) :- 
	number(X).

variable_lookup([[X,Value]|_],X,Value).
variable_lookup([[Y,_]|Rest],X,Value) :-
	X \= Y,
	variable_lookup(Rest,X,Value).

t1(O) :-
	minilisp([],[plus,2,2],O).

t2(O) :-
	minilisp([[a,2],[b,3]], [mul,2,[plus,a,b]],O).
