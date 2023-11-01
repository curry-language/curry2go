%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of allValues/someValue/oneValue of Control.Findall:
%
% Warning: in contrast to Curry's definition, this implementation
% suspends until the expression does not contain unbound global variables.
% Moreover, it is strict, i.e., it computes always all solutions even if
% only a few are actually demanded!

:- block 'Unsafe.prim_allValues'(?,?,-,?).
'Unsafe.prim_allValues'(Exp,Vals,E0,E) :-
	waitUntilGround(Exp,E0,E1),
	'Unsafe.prim_allValues_exec'(Exp,Vals,E1,E).

:- block 'Unsafe.prim_allValues_exec'(?,?,-,?).
'Unsafe.prim_allValues_exec'(Exp,Vals,E0,E) :-
	hasPrintedFailure
	 -> findall((X,E1),evalGNF(Exp,X,E0,E1),ValEs),
	    extractSolutions(ValEs,Vals,E0,E)
	  ; asserta(hasPrintedFailure),
	    findall((X,E1),evalGNF(Exp,X,E0,E1),ValEs),
	    retract(hasPrintedFailure),
	    extractSolutions(ValEs,Vals,E0,E).

% evaluate a given expression to a ground normal form,
% i.e., suspend if the normal form contains free variables.
:- block evalGNF(?,?,-,?).
evalGNF(Exp,R,E0,E1) :- nf(Exp,R,E0,E2), waitUntilGround(R,E2,E1).

% since the above implementation of allValues is strict,
% we offer also oneValue which only evaluates the first value:

:- block 'Unsafe.prim_oneValue'(?,?,-,?).
'Unsafe.prim_oneValue'(Exp,Val,E0,E) :-
	waitUntilGround(Exp,E0,E1),
	'Unsafe.prim_oneValue_exec'(Exp,Val,E1,E).

:- block 'Unsafe.prim_oneValue_exec'(?,?,-,?).
'Unsafe.prim_oneValue_exec'(Exp,Val,E0,E) :-
	hasPrintedFailure
	 -> findall((X,E1),oneNF(Exp,X,E0,E1),ValEs),
            extractSolutions(ValEs,Vals,E0,E1),
            (Vals=[X] -> Val='Prelude.Just'(X) ; Val='Prelude.Nothing'), E=E1
	  ; asserta(hasPrintedFailure),
	    findall((X,E1),oneNF(Exp,X,E0,E1),ValEs),
	    retract(hasPrintedFailure),
	    extractSolutions(ValEs,Vals,E0,E1),
            (Vals=[X] -> Val='Prelude.Just'(X) ; Val='Prelude.Nothing'), E=E1.

% evaluate a given expression to a single ground normal form,
% i.e., suspend if the normal form contains free variables and ignore
% further values.
:- block oneNF(?,?,-,?).
oneNF(Exp,R,E0,E1) :- evalGNF(Exp,R,E0,E1), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of Control.Findall.isFail:
%
% If a non-local variable is bound during the computation (for this purpose,
% they are extracted before and checked afterwards for unboundedness),
% a warning is issued for the moment.
% A better solution for the future is to replace these variables
% by generater operations.

:- block prim_isFail(?,?,-,?).
prim_isFail(Exp,Val,E0,E) :-
	hasPrintedFailure
	 -> noHNF(Exp,Val,E0,E)
	  ; asserta(hasPrintedFailure),
	    noHNF(Exp,Val,E0,E1),
	    retract(hasPrintedFailure), E1=E.

noHNF(Exp,Val,E0,E) :- \+ user:hnf(Exp,_,E0,_), !, Val='Prelude.True', E=E0.
noHNF(_,'Prelude.False',E,E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of Control.Findall.rewriteAll:
%
% To consider the evaluation or binding of non-local variables as
% a failure, they are extracted before and checked afterwards for
% unboundedness.
% Moreover, rewriteAll is strict, i.e., it evaluates always all solutions!

:- block 'Unsafe.prim_rewriteAll'(?,?,-,?).
'Unsafe.prim_rewriteAll'(Exp,Vals,E0,E) :-
	varsInExp(Exp,[],ExpVars),
	rewriteAllExec(ExpVars,Exp,Vals,E0,E1),
	E1=E.

:- block rewriteAllExec(?,?,?,-,?).
rewriteAllExec(ExpVars,Exp,Vals,E0,E) :-
	hasPrintedFailure
	 -> findall((Val,E1),
		    (user:nf(Exp,Val,E0,E1), allUnboundVariables(ExpVars)),
		    ValEs),
	    extractSolutions(ValEs,Vals,E0,E)
	  ; asserta(hasPrintedFailure),
	    findall((Val,E1),
		    (user:nf(Exp,Val,E0,E1), allUnboundVariables(ExpVars)),
		    ValEs),
	    retract(hasPrintedFailure),
	    extractSolutions(ValEs,Vals,E0,E).

% same as rewriteAll but computes only first value:
:- block 'Unsafe.prim_rewriteSome'(?,?,-,?).
'Unsafe.prim_rewriteSome'(Exp,Vals,E0,E) :-
        varsInExp(Exp,[],ExpVars),
	rewriteSomeExec(ExpVars,Exp,Vals,E0,E1),
	E1=E.

:- block rewriteSomeExec(?,?,?,-,?).
rewriteSomeExec(ExpVars,Exp,Val,E0,E) :-
	hasPrintedFailure
	 -> rewriteSomeExecWithPF(ExpVars,Exp,Val,E0,E)
	  ; asserta(hasPrintedFailure),
	    rewriteSomeExecWithoutPF(ExpVars,Exp,Val,E0,E).

rewriteSomeExecWithPF(ExpVars,Exp,R,E0,E) :-
        on_exception(_,
		     (user:nf(Exp,Val,E0,E), allUnboundVariables(ExpVars),
		      R = 'Prelude.Just'(Val)),
		     (R='Prelude.Nothing', E0=E)),
	!.
rewriteSomeExecWithPF(_,_,R,E0,E) :-
	R='Prelude.Nothing', E0=E.

rewriteSomeExecWithoutPF(ExpVars,Exp,R,E0,E) :-
	on_exception(_,
		     (user:nf(Exp,Val,E0,E), allUnboundVariables(ExpVars),
		      R = 'Prelude.Just'(Val)),
		     (R='Prelude.Nothing', E0=E)),
	retract(hasPrintedFailure), !.
rewriteSomeExecWithoutPF(_,_,R,E0,E) :-
	retract(hasPrintedFailure), !, R='Prelude.Nothing', E0=E.

% get all variables occurring in an expression:
varsInExp(X,Vs,Vs) :- var(X), varInList(X,Vs), !. % already found variable
varsInExp(X,Vs,[X|Vs]) :- var(X), !.
varsInExp(share(N),VM0,VM1) :-
	!,
	get_mutable(X,N),
	(X='$eval'(Exp) -> true ; Exp=X),
	varsInExp(Exp,VM0,VM1).
varsInExp(T,VM0,VM1) :-
	functor(T,_,N), varsInExpArgs(1,N,T,VM0,VM1).

varInList(X,[Y|_]) :- X==Y, !.
varInList(X,[_|Ys]) :- varInList(X,Ys).

varsInExpArgs(A,N,_,VM,VM) :- A>N, !.
varsInExpArgs(A,N,T,VM0,VM2) :-
	arg(A,T,ArgT),
	varsInExp(ArgT,VM0,VM1),
	A1 is A+1, varsInExpArgs(A1,N,T,VM1,VM2).

% checks whether a list contains different unbound variables:
allUnboundVariables(Vs) :-
        length(Vs,N), \+ \+ numbervars(Vs,0,N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% check whether all solutions of encapsulated search are not suspended:
extractSolutions([],[],E0,E0).
extractSolutions([(Sol,E)|SolEs],[Sol|Sols],E0,E1) :-
	extractMoreSolutions(SolEs,Sols,E,E0,E1).

:- block extractMoreSolutions(?,?,-,?,?).
extractMoreSolutions(SolEs,Sols,_,E0,E) :-
	extractSolutions(SolEs,Sols,E0,E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
