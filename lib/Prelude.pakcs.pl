%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Prolog implementation of external Prelude operations:
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of arithmetic functions:
%
'Prelude.prim_plusInt'(Y,X,R) :- R is X+Y.

'Prelude.prim_minusInt'(Y,X,R) :- R is X-Y.

'Prelude.prim_timesInt'(Y,X,R) :- R is X*Y.

'Prelude.prim_divInt'(Y,X,R) :- R is div(X,Y).

'Prelude.prim_modInt'(Y,X,R) :- isMod(R,X,Y).

'Prelude.prim_quotInt'(Y,X,R) :- R is X // Y.

'Prelude.prim_remInt'(Y,X,R) :- isRem(R,X,Y).


'Prelude.prim_plusFloat'(Y,X,R) :- R is X+Y.

'Prelude.prim_minusFloat'(Y,X,R) :- R is X-Y.

'Prelude.prim_timesFloat'(Y,X,R) :- R is X*Y.

'Prelude.prim_divFloat'(Y,X,R) :- R is X/Y.

'Prelude.prim_negateFloat'(X,R) :- R is -X.

% transform an integer into a float:
'Prelude.prim_intToFloat'(X,R) :- R is X*1.0.

% transform a float to an integer:
'Prelude.prim_truncateFloat'(X,R) :- R is integer(X).

% round a float to an integer:
'Prelude.prim_roundFloat'(X,R) :- R is integer(round(X)).

'Prelude.prim_sqrtFloat'(X,R) :- R is sqrt(X).

'Prelude.prim_logFloat'(X,R) :- R is log(X).

'Prelude.prim_expFloat'(X,R) :- R is exp(X).

'Prelude.prim_sinFloat'(X,R) :- R is sin(X).

'Prelude.prim_cosFloat'(X,R) :- R is cos(X).

'Prelude.prim_tanFloat'(X,R) :- R is tan(X).

'Prelude.prim_asinFloat'(X,R) :- R is asin(X).

'Prelude.prim_acosFloat'(X,R) :- R is acos(X).

'Prelude.prim_atanFloat'(X,R) :- R is atan(X).

'Prelude.prim_sinhFloat'(X,R) :- R is sinh(X).

'Prelude.prim_coshFloat'(X,R) :- R is cosh(X).

'Prelude.prim_tanhFloat'(X,R) :- R is tanh(X).

'Prelude.prim_asinhFloat'(X,R) :- R is asinh(X).

'Prelude.prim_acoshFloat'(X,R) :- R is acosh(X).

'Prelude.prim_atanhFloat'(X,R) :- R is atanh(X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of comparsion of primitive data:
%
'Prelude.prim_eqInt'(Y,X,R) :- X==Y -> R='Prelude.True' ; R='Prelude.False'.

'Prelude.prim_eqFloat'(Y,X,R) :- X==Y -> R='Prelude.True' ; R='Prelude.False'.

'Prelude.prim_eqChar'(Y,X,R) :- X==Y -> R='Prelude.True' ; R='Prelude.False'.

'Prelude.prim_ltEqInt'(Y,X,R) :- X=<Y -> R='Prelude.True' ; R='Prelude.False'.

'Prelude.prim_ltEqFloat'(Y,X,R) :- X=<Y -> R='Prelude.True' ; R='Prelude.False'.

'Prelude.prim_ltEqChar'(Y,X,R) :-
	char_int(X,VX), char_int(Y,VY),
	VX=<VY -> R='Prelude.True' ; R='Prelude.False'.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of conversion functions for characters:
%
'Prelude.prim_ord'(C,N) :- char_int(C,N).

'Prelude.prim_chr'(N,C) :- N>=0, N<1114112, !, char_int(C,N).
'Prelude.prim_chr'(_,_) :- raise_exception('chr: argument out of range').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Show and read functions for primitive types:
%
'Prelude.prim_showIntLiteral'(N,S) :- prim_showNumber(N,S).

'Prelude.prim_showFloatLiteral'(N,S) :- prim_showNumber(N,S).

prim_showNumber(N,S) :-
	number_codes(N,SN), map2M(basics:char_int,String,SN),
	(N>=0 -> S = String
	       ; % enclose negative number in parentheses:
	         char_int(Op,40), char_int(Cl,41),
	         append([Op|String],[Cl],S)).

% conversion of string representations of nat literals into Curry terms:
'Prelude.prim_readNatLiteral'([CC|String],['Prelude.(,)'(Num,TailString)]) :-
	char_int(CC,C), C>47, C<58,
        natConst(NumStr,String,TailString),
        number_codes(Num,[C|NumStr]), !.
'Prelude.prim_readNatLiteral'(_,[]). % parse error

% conversion of string representations of float literals into Curry terms:
'Prelude.prim_readFloatLiteral'([CC|String],['Prelude.(,)'(Num,TailString)]) :-
	char_int(CC,C), C>47, C<58,
        floatConst(NumStr,String,TailString),
        number_codes(Num,[C|NumStr]), !.
'Prelude.prim_readFloatLiteral'(_,[]). % parse error

natConst([C|Cs]) --> [CC],
        { char_int(CC,C), C>47, C<58 }, !,
        natConst(Cs).
natConst([]) --> skipblanks.

floatConst([C|Cs]) --> [CC],
        { char_int(CC,C), C>47, C<58 }, !,
        floatConst(Cs).
floatConst([46,C|Cs]) --> [PC], { char_int(PC,46) }, [CC],
        { char_int(CC,C), C>47, C<58 }, !,
        floatConstRest(Cs).

floatConstRest([C|Cs]) --> [CC],
        { char_int(CC,C), C>47, C<58 }, !,
        floatConstRest(Cs).
floatConstRest([C|Cs]) --> [CC], {char_int(CC,C), C=69 ; C=101}, !, % exponent
	intConst(Cs).
floatConstRest([]) --> skipblanks.

intConst(Cs) --> ( [CC], {char_int(CC,45)}, natConst(NCs), {Cs=[45|NCs]}
		  ; natConst(Cs)
		  ).


% conversion of string representations of char literals into Curry terms:
% TODO: avoid char_int conversion
'Prelude.prim_readCharLiteral'(String,['Prelude.(,)'(Char,TailString)]) :-
	map2M(basics:char_int,String,[C|PrologString]),
	C=39, readChar(PrologString,Tail,Char),
	map2M(basics:char_int,TailString,Tail), !.
'Prelude.prim_readCharLiteral'(_,[]). % parse error

% conversion of string representations of string literals into Curry terms:
% TODO: avoid char_int conversion
'Prelude.prim_readStringLiteral'(String,['Prelude.(,)'(Result,TailString)]) :-
	map2M(basics:char_int,String,[C|PrologString]),
	C=34, readString(PrologString,Tail,Result),
	map2M(basics:char_int,TailString,Tail), !.
'Prelude.prim_readStringLiteral'(_,[]). % parse error

'Prelude.prim_showCharLiteral'(C,[Apo|S]) :-
	char_int(Apo,39),  % 39='''
	char_int(C,N),
	(N=39 -> char_int(BS,92), S=[BS,C|SE] % '
	 ; (N=34 -> S=[C|SE] % "
	          ; showTermChar(N,S,SE))),
	SE = [Apo].

'Prelude.prim_showStringLiteral'(Str,[Quot|S]) :-
	char_int(Quot,34), % 34 = '"'
	showTermString(Str,S,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of I/O actions:
%
'Prelude.prim_putChar'(C,'Prelude.()') :-
	char_int(C,N), put_code(N),
	%flush_output. % this is problematic for Sicstus4 (substantial delay)
	(N=10 -> flush_output ; true).

'Prelude.getChar'(C) :- get_code(N), char_int(C,N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% handling "FAIL" values

% Checks whether a Prolog term is a "FAIL" value.
isFail(T) :- nonvar(T), T='FAIL'(_).

% Check whether a Prolog term is a "FAIL" value and, if yes, extend it
% with one item:
?- block checkFailValue(?,?,?,-,?).
checkFailValue(Ext,Val,Result,E0,E) :-
	(nonvar(Val), Val='FAIL'(Src))
	 -> Result='FAIL'([Ext|Src]), E0=E
	  ; Result=Val, E0=E.

% normalize a term and show "FAIL" errors, if necessary:
?- block normalizeAndCheck(?,?,-,?).
normalizeAndCheck(Exp,Val,E0,E) :-
	user:nf(Exp,Val,E0,E1),
	normalizeAndCheckNF(Val,E1,E).

?- block normalizeAndCheckNF(?,-,?).
normalizeAndCheckNF(Val,E0,E) :-
	isFail(Val) -> Val='FAIL'(S), evaluator:writeFailSource(S)
	             ; E0=E.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% definition of concurrent conjunction:
?- block prim_concurrent_and(?,?,?,-,?).
prim_concurrent_and(C1,C2,R,E0,E) :-
      user:hnf(C1,S1,E0,E1),
      user:hnf(C2,S2,E0,E2),
      waitConcurrentConjunction(S1,S2,R,E1,E2,E).

% primitive for conditional rules:
?- block prim_cond(?,?,?,-,?).
prim_cond(Cond,RHS,R,E0,E) :-
	user:hnf(Cond,S,E0,E1), prim_checkcond(S,Cond,RHS,R,E1,E).

?- block prim_checkcond(-,?,?,?,?,?), prim_checkcond(?,?,?,?,-,?).
prim_checkcond('Prelude.True',_,RHS,R,E0,E) :- user:hnf(RHS,R,E0,E).
prim_checkcond('FAIL'(Src),Cond,RHS,'FAIL'(['Prelude.cond'(Cond,RHS)|Src]),E,E).


% primitive for implementing letrec:
?- block prim_letrec(?,?,?,-,?).
prim_letrec(X,XE,'Prelude.True',E0,E) :- var(XE), !, X=XE, E0=E.
prim_letrec(X,XE,'Prelude.True',E0,E) :- create_mutable(XE,MX), X=share(MX), E0=E.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of I/O actions:
%
?- block prim_Monad_bind(?,?,?,-,?).
prim_Monad_bind(A1,FA2,partcall(1,prim_Monad_bindWorld,[FA2,A1]),E,E).

?- block prim_Monad_bindWorld(?,?,?,?,-,?).
prim_Monad_bindWorld(Action1,FunAction2,W,R,E0,E) :-
        prim_apply(Action1,W,'$io'(R1),E0,E1),
	prim_apply(FunAction2,R1,HAction2,E1,E2),
	prim_apply(HAction2,W,R,E2,E).


% Although the IO instance of (*>), having the internal name
% "_impl#*>#Prelude.Applicative#Prelude.IO"
% (and, thus, the IO instance of (>>)) is a defined function,
% we provide a slightly faster primitive implementation.
% The usage of this implementation is specified in Prelude.pakcs
?- block prim_Monad_seq(?,?,?,-,?).
prim_Monad_seq(A1,A2,partcall(1,prim_Monad_seqWorld,[A2,A1]),E,E).

?- block prim_Monad_seqWorld(?,?,?,?,-,?).
prim_Monad_seqWorld(Action1,Action2,W,R,E0,E) :-
        prim_apply(Action1,W,_,E0,E1),
	prim_apply(Action2,W,R,E1,E).


?- block prim_return(?,?,-,?).
prim_return(V,partcall(1,prim_returnWorld,[V]),E,E).

?- block prim_returnWorld(?,?,?,-,?).
prim_returnWorld(A,_,'$io'(A),E,E).


'Prelude.prim_readFile'(A,Result) :-
	string2Atom(A,FName),
	fileOpenOptions(Options),
	open(FName,read,Stream,Options),
	(compileWithSharing(function)
	 -> makeShare('Prelude.prim_readFileContents'(Stream),Result)
	  ; Result = 'Prelude.prim_readFileContents'(Stream)).

?- block prim_readFileContents(?,?,-,?).
prim_readFileContents(Stream,Result,E0,E) :-
	atEndOfStream(Stream), !,
	Result=[],
	close(Stream), E0=E.
prim_readFileContents(Stream,Result,E0,E) :-
	get_code(Stream,NChar),
	char_int(Char,NChar),
	Result=[Char|RFC],
	(compileWithSharing(function)
	 -> makeShare('Prelude.prim_readFileContents'(Stream),RFC)
	  ; RFC = 'Prelude.prim_readFileContents'(Stream)),
	E0=E.


?- block prim_writeFile(?,?,?,-,?).
prim_writeFile(F,S,partcall(1,prim_writeFileWorld,[S,F]),E,E).

?- block prim_writeFileWorld(?,?,?,?,-,?).
prim_writeFileWorld(RA,S,W,H,E0,E) :- user:derefAll(RA,A),
	string2Atom(A,FName),
	fileOpenOptions(Options),
	open(FName,write,Stream,Options),
	prim_writeFileContents(Stream,S,W,H,E0,E).

?- block prim_appendFile(?,?,?,-,?).
prim_appendFile(F,S,partcall(1,prim_appendFileWorld,[S,F]),E,E).

?- block prim_appendFileWorld(?,?,?,?,-,?).
prim_appendFileWorld(RA,S,W,H,E0,E) :- user:derefAll(RA,A),
	string2Atom(A,FName),
	fileOpenOptions(Options),
	open(FName,append,Stream,Options),
	prim_writeFileContents(Stream,S,W,H,E0,E).

?- block prim_writeFileContents(?,?,?,?,-,?).
prim_writeFileContents(Stream,Contents,W,R,E0,E) :-
	user:hnf(Contents,HContents,E0,E1),
	prim_writeFileContents1(HContents,Stream,W,R,E1,E).

?- block prim_writeFileContents1(-,?,?,?,?,?),
         prim_writeFileContents1(?,?,?,?,-,?).
prim_writeFileContents1([],Stream,_,'$io'('Prelude.()'),E0,E) :-
	flush_output(Stream), close(Stream), E0=E.
prim_writeFileContents1([C|Cs],Stream,W,R,E0,E) :-
	user:hnf(C,HC,E0,E1),
	put_writeFileContents(Stream,HC,Cs,W,R,E1,E).
prim_writeFileContents1('FAIL'(Src),_,_,'FAIL'(Src),E,E).

?- block put_writeFileContents(?,-,?,?,?,?,?),
         put_writeFileContents(?,?,?,?,?,-,?).
put_writeFileContents(_,'FAIL'(Src),_,_,'FAIL'(Src),E,E) :- !.
put_writeFileContents(Stream,C,Cs,W,H,E0,E) :-
	char_int(C,N), put_code(Stream,N),
	prim_writeFileContents(Stream,Cs,W,H,E0,E).


?- block prim_catch(?,?,?,-,?).
prim_catch(A1,A2,partcall(1,prim_catchWorld,[A2,A1]),E,E).

?- block prim_catchWorld(?,?,?,?,-,?).
prim_catchWorld(Action,ErrFunction,W,R,E0,E) :-
	on_exception(ErrorMsg,
		     (prim_apply(Action,W,R0,E0,E),
                         (nonvar(E) -> R=R0
                         ; ErrAtom = 'Computation suspended',
                           returnIOError(ErrAtom,ErrFunction,W,R,E0,E))),
		     (prologError2Atom(ErrorMsg,ErrAtom),
		      returnIOError(ErrAtom,ErrFunction,W,R,E0,E))),
	!.
prim_catchWorld(_,ErrFunction,W,R,E0,E) :-
	atom2String('IO action failed',FailMsg),
	applyErrorFunction(ErrFunction,'Prelude.FailError'(FailMsg),W,R,E0,E).

returnIOError(ErrAtom,ErrFunction,W,R,E0,E) :-
        atom2String(ErrAtom,ErrString),
        ErrValue = 'Prelude.IOError'(ErrString),
        applyErrorFunction(ErrFunction,ErrValue,W,R,E0,E).

applyErrorFunction(ErrFunction,ErrValue,W,R,E0,E) :-
	prim_apply(ErrFunction,ErrValue,ErrAction,E0,E1),
	prim_apply(ErrAction,W,R,E1,E).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% definition of apply primitive:
?- block prim_apply(?,?,?,-,?).
prim_apply(F,X,R,E0,E) :- user:hnf(F,HF,E0,E1), prim_hnf_apply(HF,X,R,E1,E).

?- block prim_hnf_apply(-,?,?,?,?), prim_hnf_apply(?,?,?,-,?).
prim_hnf_apply('FAIL'(Src),_,'FAIL'(Src),E,E) :- !.
prim_hnf_apply(partcall(N,F,Args),X,R,E0,E) :- !,
	(N=1 -> (X=='$world' % application of primitive IO operation
		 -> rev([E,E0,R,X|Args],AllArgs),
		    %(F=M:UF -> true ; F=UF, M=user),
		    Term =.. [F|AllArgs], call(user:Term)
		  ; prim_hnf_apply_call(Args,F,X,R,E0,E))
	      ; N1 is N-1, R=partcall(N1,F,[X|Args]), E0=E).
prim_hnf_apply('Dynamic.Dynamic'(DP),X,'Dynamic.Dynamic'(DPX),E0,E) :- !,
	% special treatment of dynamic predicates:
	user:hnf(DP,DynPred,E0,E1),
	DynPred =.. [P|Args],
	append(Args,[X],ArgsX), % not constant time! (TODO: improve)
	DPX =.. [P|ArgsX],
	E1=E.
prim_hnf_apply(CTerm,X,R,E0,E) :- % partial constructor applications
	CTerm =.. [C|Args],
	append(Args,[X],ArgsX), % not constant time, hopefully occurs not so often,
	R =.. [C|ArgsX],        % otherwise one can represent them also as partcalls
	E0=E.

prim_hnf_apply_call([],F,X,R,E0,E) :- !, Term =.. [F,X], user:hnf(Term,R,E0,E).
prim_hnf_apply_call([A1],F,X,R,E0,E) :- !, Term =.. [F,A1,X], user:hnf(Term,R,E0,E).
prim_hnf_apply_call([A1,A2],F,X,R,E0,E) :- !, Term =.. [F,A2,A1,X], user:hnf(Term,R,E0,E).
prim_hnf_apply_call([A1,A2,A3],F,X,R,E0,E) :- !, Term =.. [F,A3,A2,A1,X], user:hnf(Term,R,E0,E).
prim_hnf_apply_call(Args,F,X,R,E0,E) :- % the general case:
	rev([X|Args],AllArgs), Term =.. [F|AllArgs], user:hnf(Term,R,E0,E).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of $!
?- block prim_applySeq(?,?,?,-,?).
prim_applySeq(F,X,R,E0,E) :-
	user:hnf(X,HX,E0,E1),
	prim_applySeqHNF(F,HX,R,E1,E).

?- block prim_applySeqHNF(?,?,?,-,?).
prim_applySeqHNF(F,HX,R,E0,E) :-
	isFail(HX) -> R=HX, E0=E
	            ; user:hnf(F,HF,E0,E1), prim_hnf_apply(HF,HX,R,E1,E).

% Implementation of $!!
?- block prim_applyNormalForm(?,?,?,-,?).
prim_applyNormalForm(F,X,R,E0,E) :-
	user:nf(X,NX,E0,E1),
	prim_applyNormalFormNF(F,NX,R,E1,E).

?- block prim_applyNormalFormNF(?,?,?,-,?).
prim_applyNormalFormNF(F,NX,R,E0,E) :-
	isFail(NX) -> R=NX, E0=E
	            ; user:hnf(F,HF,E0,E1), prim_hnf_apply(HF,NX,R,E1,E).

% Implementation of $#
?- block prim_applyNotFree(?,?,?,-,?).
prim_applyNotFree(F,X,R,E0,E) :-
	user:hnf('Prelude.ensureNotFree'(X),HX,E0,E1),
	prim_applyNotFreeHNF(F,HX,R,E1,E).

?- block prim_applyNotFreeHNF(?,?,?,-,?).
prim_applyNotFreeHNF(F,HX,R,E0,E) :-
	isFail(HX) -> R=HX, E0=E
	            ; user:hnf(F,HF,E0,E1), prim_hnf_apply(HF,HX,R,E1,E).

% Implementation of $##
?- block prim_applyGroundNormalForm(?,?,?,-,?).
prim_applyGroundNormalForm(F,X,R,E0,E) :-
	user:nf(X,NX,E0,E1),
	waitUntilGround(NX,E1,E2),
	prim_applyGroundNormalFormNF(F,NX,R,E2,E).

?- block prim_applyGroundNormalFormNF(?,?,?,-,?).
prim_applyGroundNormalFormNF(F,NX,R,E0,E) :-
	isFail(NX) -> R=NX, E0=E
	            ; user:hnf(F,HF,E0,E1), prim_hnf_apply(HF,NX,R,E1,E).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% definition of seq primitive:
?- block prim_seq(?,?,?,-,?).
prim_seq(Arg,Val,Result,E0,E) :-
	user:hnf(Arg,H,E0,E1), prim_seqHNF(H,Val,Result,E1,E).

?- block prim_seqHNF(?,?,?,-,?).
prim_seqHNF(H,Val,Result,E0,E) :-
	isFail(H) -> Result=H, E0=E
	           ; user:hnf(Val,Result,E0,E).

% definition of ensureNotFree primitive (rigid on first argument):
?- block prim_ensureNotFree(?,?,-,?).
prim_ensureNotFree(Arg,Result,E0,E) :-
	user:hnf(Arg,Val,E0,E1),
	prim_ensureNotFreeHNF(Val,Result,E1,E).

?- block prim_ensureNotFreeHNF(?,?,-,?).
prim_ensureNotFreeHNF(Val,Result,E0,E) :-
	isFail(Val)
	 -> Result=Val, E0=E
	  ; (var(Val) -> addSuspensionReason('Applying a primitive (rigid) operation to a free variable') ; true),
	    prim_ensureHnfNotFree(Val,Result,E0,E).

?- block prim_ensureHnfNotFree(-,?,?,?), prim_ensureHnfNotFree(?,?,-,?).
prim_ensureHnfNotFree(Val,Val,E,E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% definition of run-time error and failure:
'Prelude.prim_error'(Msg,_) :-
	string2Atom(Msg,AMsg),
	raise_exception(AMsg).

?- block prim_failed(?,-,?).
prim_failed(R,E0,E) :- prim_failure(partcall(0,'Prelude.failed',[]),[],R,E0,E).
%prim_failed(_,E,E) :- fail.

?- block prim_failure(?,?,?,-,?).
prim_failure(_,_,_,_,_) :- printConsFailure(no), !, fail. % no reporting required
prim_failure(_,_,_,_,_) :-  % no reporting in findall:
	hasPrintedFailure, !, fail.
prim_failure(PartCall,ConsExp,Result,E0,E) :- % generate FAIL value
	Result = 'FAIL'([PartCall,ConsExp]),
	E0=E.
%prim_failure(PartCall,ConsExp,_,E,E) :- evaluator:writeFailSource([PartCall,ConsExp]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO: implement prim_compare in main Prolog file similarly to hnf,nf,...
% so that its computations are also covered by the profiler.
?- block prim_compare(?,?,?,-,?).
prim_compare(X,Y,R,E0,E) :-
	user:hnf(X,HX,E0,E1),
	user:hnf(Y,HY,E1,E2),
	prim_compareHNF(HX,HY,R,E2,E).

?- block prim_compareHNF(?,?,?,-,?).
prim_compareHNF(X,Y,R,E0,E) :- var(X), var(Y), !,
	addSuspensionReason('Comparing (with <, >,...) two free variables'),
	when((nonvar(X);nonvar(Y)), prim_compareHNF(X,Y,R,E0,E)).
prim_compareHNF(X,Y,R,E0,E) :- var(X), !,
	prim_compareHNF(Y,X,R0,E0,E1),
	switchOrdering(R0,R), E1=E.
prim_compareHNF('FAIL'(Src),_,'FAIL'(Src),E,E) :- !.
prim_compareHNF(_,Y,R,E0,E) :- nonvar(Y), Y='FAIL'(_), !, R=Y, E0=E.
prim_compareHNF(X,Y,R,E0,E) :- var(Y), (number(X); isCharCons(X)), !,
	addSuspensionReason('Comparing (with <, >,...) a free variable with a number or character'),
	when(nonvar(Y), prim_compareHNF(X,Y,R,E0,E)).
prim_compareHNF(X,Y,R,E0,E) :- number(X), !,
	(X=Y -> R='Prelude.EQ' ; (X<Y -> R='Prelude.LT' ; R='Prelude.GT')),
	E0=E.
prim_compareHNF(X,Y,R,E0,E) :- isCharCons(X), !,
	char_int(X,VX), char_int(Y,VY),
	(VX=VY -> R='Prelude.EQ' ; (VX<VY -> R='Prelude.LT' ; R='Prelude.GT')),
	E0=E.
prim_compareHNF(X,Y,R,E0,E) :- var(Y), !,
	functor(X,FX,NX),
	( functor(Y,FX,NX), prim_compareArgs(1,NX,X,Y,R,E0,E)
	; user:constructortype(FX,_,NX,_,IX,_,OtherCons),
	  member(FY/NY,OtherCons),
	  user:constructortype(FY,_,NY,_,IY,_,_),
	  functor(Y,FY,NY),
	  (IX<IY -> R='Prelude.LT', E0=E ; (IX>IY -> R='Prelude.GT', E0=E))
	).
prim_compareHNF(X,Y,R,E0,E) :-
	functor(X,FX,NX), functor(Y,FY,NY),
	user:constructortype(FX,_,NX,_,IX,_,_),
	user:constructortype(FY,_,NY,_,IY,_,_), !,
	(IX<IY -> R='Prelude.LT', E0=E ; (IX>IY -> R='Prelude.GT', E0=E
          ; prim_compareArgs(1,NX,X,Y,R,E0,E))).

?- block prim_compareArgs(?,?,?,?,?,-,?).
prim_compareArgs(I,N,_,_,R,E0,E) :- I>N, !, R='Prelude.EQ', E0=E.
prim_compareArgs(I,N,X,Y,R,E0,E) :-
	arg(I,X,ArgX), arg(I,Y,ArgY),
	prim_compare(ArgX,ArgY,ArgR,E0,E1),
	(ArgR='Prelude.EQ' -> I1 is I+1, prim_compareArgs(I1,N,X,Y,R,E1,E)
	                    ; R=ArgR, E1=E).

switchOrdering('Prelude.LT','Prelude.GT') :- !.
switchOrdering('Prelude.GT','Prelude.LT') :- !.
switchOrdering(X,X).


:- block waitUntilGround(-,?,?), waitUntilGround(?,-,?).
waitUntilGround(share(M),E0,E) :-
	!,
	get_mutable(V,M),
	(V='$eval'(Exp) -> true ; Exp=V),
	waitUntilGround(Exp,E0,E).
waitUntilGround(T,E0,E) :- functor(T,_,N), waitUntilGroundArgs(1,N,T,E0,E).

:- block waitUntilGroundArgs(?,?,?,-,?).
waitUntilGroundArgs(A,N,_,E0,E) :- A>N, !, E0=E.
waitUntilGroundArgs(A,N,T,E0,E) :-
	arg(A,T,Arg), waitUntilGround(Arg,E0,E1),
	A1 is A+1, waitUntilGroundArgs(A1,N,T,E1,E).


% compute head normal form and wait until it is ground:
:- block hnfAndWaitUntilGround(?,?,-,?).
hnfAndWaitUntilGround(X,HX,E0,E) :-
	user:hnf(X,HX,E0,E1),
	hnfAndWaitUntilGroundHNF(HX,E1,E).

:- block hnfAndWaitUntilGroundHNF(?,-,?).
hnfAndWaitUntilGroundHNF(X,E0,E) :-
	isFail(X) -> E0=E
	           ; waitUntilGround(X,E0,E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Directed non-strict equality for matching against functional patterns:
% (first argument must be the functional pattern):
:- block 'Prelude.nonstrictEq'(?,?,?,-,?).
'Prelude.nonstrictEq'(A,B,R,E0,E):-
        user:hnf(A,HA,E0,E1), unifEq1(HA,B,R,E1,E).

:- block unifEq1(?,?,?,-,?).
% In the following clause, we bind a functional pattern variable to the
% actual argument. This binding of a logical variable against
% a non-constructor term is not problematic since the functional pattern
% variable is a logical variable that is not enclosed
% by a sharing structure (compare definition of makeShare).
unifEq1(FPat,ActArg,'Prelude.True',E0,E) :-
	var(FPat), !,
	user:occursNot(FPat,ActArg),
	%FPat=ActArg, % this would implement run-time choice
	% in order to implement call-time choice for pattern variables,
	% we wrap the pattern variable in a share structure
	% (this could be optimized by checking the number of further occurrences
	% of the pattern variable)
	makeShare(ActArg,FPat),
	%writeErr('BOUND TO: '), removeShares(ActArg,AA), writeErr(AA), nlErr,
	E0=E.
unifEq1('FAIL'(Src),_,'FAIL'(Src),E,E):- !.
unifEq1(A,B,R,E0,E) :-
	replaceMultipleVariables(A,LinA,LinConstraints),
	user:hnf(B,HB,E0,E1),
	unifEqHnf(LinA,HB,EqR,E1,E2),
	unifEq2(EqR,LinConstraints,R,E2,E).

:- block unifEq2(?,?,?,-,?).
unifEq2(EqR,LinConstraints,R,E0,E) :-
	isFail(EqR)
	-> R=EqR, E0=E
	 ; %(LinConstraints='Prelude.True' -> true
	   % ; writeErr('Linearity constraints: '),
	   %   writeErr(LinConstraints), nlErr),
	user:hnf(LinConstraints,R,E0,E).

% replace multiple occurrences of a same logic variables by new ones combined
% with strict equations:
replaceMultipleVariables(T,LinT,LinConstraints) :-
	%writeErr('Term to linearize: '), writeErr(T), nlErr,
	T =.. [Cons|Args],
	replaceMultipleVariablesInArgs(Args,inConstructorCall,Vars,LinArgs),
	LinT =.. [Cons|LinArgs],
	getSEqConstraints(Vars,LinConstraints).
	%length(Vars,Len), writeErr('Length of variable list: '), writeErr(Len), nlErr.

getControlVar(X,Below,L,NewX) :- var(L), !, L=[control(X,Below,NewX,_)|_].
getControlVar(X,Below,[control(Y,YBelow,NewVar,NewConstraint)|_],NewX) :- X==Y, !,
	% multiple occurrence of a variable X not inside function calls are replaced
	% a fresh variable Y and a strict equality constraint X=:=Y
	% that is later executed.
	((Below=inConstructorCall, YBelow=inConstructorCall)
	 -> (var(NewConstraint)
	     -> NewConstraint = 'Prelude.constrEq'(X,NewVar), NewX=X
 	      ; NewX=NewVar)
	  ; % multiple occurrence of a variable X, where one occurrence is in a
	    % function call, are replaced by an expression that
	    % forces the evaluation of a strict equality constraint if the variable
	    % occurs multiple times in the finally evaluated pattern.
	    % Therefore, each variable X is replaced by
	    % (if isVar ShareX then ShareX=:=() else CtrlX=:=()) &> X
	    % with a constraint
	    % (if isVar CtrlX then success else X=:=X) that is later executed.
	    NewX=NewVar,
	    (var(NewConstraint)
	     -> NewVar = 'Prelude.&>'('Prelude.ifVar'(ShareVar,
				      'Prelude.constrEq'(ShareVar,'Prelude.()'),
				      'Prelude.constrEq'(CtrlVar,'Prelude.()')),X),
	        NewConstraint = 'Prelude.ifVar'(CtrlVar,
						'Prelude.True',
						'Prelude.constrEq'(X,X))
 	      ; true)).
getControlVar(X,Below,[_|L],NewVar) :- getControlVar(X,Below,L,NewVar).

getSEqConstraints(L,'Prelude.True') :- var(L), !, L=[].
getSEqConstraints([control(X,_,NewVar,NewConstraint)|L],Constraints) :-
	var(NewConstraint), !, % occurred only once
	X=NewVar,
	getSEqConstraints(L,Constraints).
getSEqConstraints([control(_,_,_,NewConstraints)|L],
		  'Prelude.&'(NewConstraints,Constraints)) :-
	getSEqConstraints(L,Constraints).

replaceMultipleVariablesInArgs([],_,_,[]).
replaceMultipleVariablesInArgs([X|Args],Below,Vars,[NewArg|LinArgs]) :-
	var(X), !, getControlVar(X,Below,Vars,NewArg),
	replaceMultipleVariablesInArgs(Args,Below,Vars,LinArgs).
replaceMultipleVariablesInArgs([Arg|Args],Below,Vars,[Arg|LinArgs]) :-
	% avoid repeating replacing already replaced variables
	Arg = 'Prelude.&>'('Prelude.ifVar'(ShareVar,
				      'Prelude.constrEq'(ShareVar,'Prelude.()'),
				      'Prelude.constrEq'(_CtrlVar,'Prelude.()')),_),
        !,
	replaceMultipleVariablesInArgs(Args,Below,Vars,LinArgs).
replaceMultipleVariablesInArgs([Arg|Args],Below,Vars,[LinArg|LinArgs]) :-
	Arg =.. [FC|Ts],
	(user:functiontype(FC,_,_,_,_,_) -> TsBelow= inFunctionCall ; TsBelow=Below),
	replaceMultipleVariablesInArgs(Ts,TsBelow,Vars,LinTs),
   	LinArg =.. [FC|LinTs],
	replaceMultipleVariablesInArgs(Args,Below,Vars,LinArgs).

:- block unifEqHnf(?,?,?,-,?).
unifEqHnf(A,B,Success,E0,E) :- var(B),!,
	user:bind(B,A,Success,E0,E).  % in order to evaluate function pattern
unifEqHnf(_,'FAIL'(Src),'FAIL'(Src),E,E) :- !.
unifEqHnf(A,B,R,E0,E) :-
	number(A), !,
	(A=B -> R='Prelude.True', E0=E
	      ; prim_failure(partcall(2,'Prelude.=:<=',[]),[A,B],R,E0,E)).
unifEqHnf(A,B,R,E0,E) :-
	functor(A,FuncA,ArA), functor(B,FuncB,ArB), FuncA==FuncB, ArA==ArB, !,
	genUnifEqHnfBody(1,ArA,A,B,Con), user:hnf(Con,R,E0,E).
unifEqHnf(A,B,R,E0,E) :-
	prim_failure(partcall(2,'Prelude.=:<=',[]),[A,B],R,E0,E).

genUnifEqHnfBody(N,Arity,_,_,'Prelude.True') :- N>Arity, !.
genUnifEqHnfBody(N,Arity,A,B,'Prelude.nonstrictEq'(ArgA,ArgB)):-
	N=Arity, !,
	arg(N,A,ArgA), arg(N,B,ArgB).
genUnifEqHnfBody(N,Arity,A,B,'Prelude.&'('Prelude.nonstrictEq'(ArgA,ArgB),G)):-
	arg(N,A,ArgA), arg(N,B,ArgB),
	N1 is N+1,
	genUnifEqHnfBody(N1,Arity,A,B,G).

% Directed non-strict equality for matching against linear function patterns,
% i.e., it must be ensured that the first argument is always (after evalutation
% by narrowing) a linear pattern.
% At call time, the first argument must be the function pattern.
:- block unifEqLinear(?,?,?,-,?).
unifEqLinear(A,B,R,E0,E):-
	user:hnf(A,HA,E0,E1), unifEqLinear1(HA,B,R,E1,E).

:- block unifEqLinear1(?,?,?,-,?).
% In the following clause, we bind a function pattern variable to the
% actual argument. This binding of a logical variable against
% a non-constructor term is not problematic since the functional pattern
% variable is a logical variable that is not enclosed
% by a sharing structure (compare definition of makeShare).
unifEqLinear1(FPat,ActArg,'Prelude.True',E0,E):-
	var(FPat), !,
	%FPat=ActArg, % this would implement run-time choice
	% in order to implement call-time choice for pattern variables,
	% we wrap the pattern variable in a share structure
	% (this could be optimized by checking the number of further occurrences
	% of the pattern variable)
	makeShare(ActArg,FPat),
	%writeErr('BOUND TO: '), removeShares(ActArg,AA), writeErr(AA), nlErr,
	E0=E.
unifEqLinear1('FAIL'(Src),_,'FAIL'(Src),E,E):- !.
unifEqLinear1(A,B,R,E0,E):-
	user:hnf(B,HB,E0,E1), unifEqLinearHnf(A,HB,R,E1,E).

:- block unifEqLinearHnf(?,?,?,-,?).
unifEqLinearHnf(A,B,R,E0,E) :- var(B), !,
	user:nf(A,NA,E0,E1),
	freeze(E1,(isFail(NA) -> R=NA, E1=E ; B=NA, R='Prelude.True', E1=E)).
unifEqLinearHnf(_,'FAIL'(Src),'FAIL'(Src),E,E) :- !.
unifEqLinearHnf(A,B,R,E0,E) :-
	number(A), !,
	(A=B -> R='Prelude.True', E0=E
	      ; prim_failure(partcall(2,'Prelude.=:<<=',[]),[A,B],R,E0,E)).
unifEqLinearHnf(A,B,R,E0,E) :-
	functor(A,FuncA,ArA), functor(B,FuncB,ArB), FuncA==FuncB, ArA==ArB, !,
	genUnifEqLinearHnfBody(1,ArA,A,B,Con), user:hnf(Con,R,E0,E).
unifEqLinearHnf(A,B,R,E0,E) :-
	prim_failure(partcall(2,'Prelude.=:<<=',[]),[A,B],R,E0,E).

genUnifEqLinearHnfBody(N,Arity,_,_,'Prelude.True') :- N>Arity, !.
genUnifEqLinearHnfBody(N,Arity,A,B,'Prelude.unifEqLinear'(ArgA,ArgB)):-
	N=Arity, !,
	arg(N,A,ArgA), arg(N,B,ArgB).
genUnifEqLinearHnfBody(N,Arity,A,B,
                       'Prelude.&'('Prelude.unifEqLinear'(ArgA,ArgB),G)):-
	arg(N,A,ArgA), arg(N,B,ArgB),
	N1 is N+1,
	genUnifEqLinearHnfBody(N1,Arity,A,B,G).


% ifVar x t f  corresponds to  if (Unsafe.isVar x) then t else f:
?- block prim_ifVar(?,?,?,?,-,?).
prim_ifVar(RTerm,T,F,H,E0,E) :-
	user:derefRoot(RTerm,Term),
        (var(Term) -> user:hnf(T,H,E0,E) ; user:hnf(F,H,E0,E)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
