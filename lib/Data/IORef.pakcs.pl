%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Prolog implementation of builtins of module Data.IORef:
%

% New IORefs are represented as mutable values. The "share" constructor
% is put around to be conform with the remaining implementation where
% all mutables are "marked" by this constructor.

?- block prim_newIORef(?,?,-,?).
prim_newIORef(V,partcall(1,exec_newIORef,[V]),E,E).
?- block exec_newIORef(?,?,?,-,?).
exec_newIORef(Val,_,'$io'('IOExts.IORef'(share(MutVal))),E0,E) :-
        var(Val), !,
	create_mutable('$eval'(Val),MutVal), E0=E.
exec_newIORef(Val,_,'$io'('IOExts.IORef'(share(MutVal))),E0,E) :-
	create_mutable(Val,MutVal), E0=E.

% When an IORef is read and its value is not evaluated, the current value
% is wrapped into a new mutable in order to implement sharing
% of evaluations of IORefs. The current IORef is updated so that
% it refers to the new mutable (without this indirection, there is
% a risk of creating cyclic structures when the IORef itself is updated).

?- block prim_readIORef(?,?,-,?).
prim_readIORef(R,partcall(1,exec_readIORef,[R]),E,E).
?- block exec_readIORef(?,?,?,-,?).
exec_readIORef(RIORef,_,'$io'(V),E0,E) :-
        user:derefRoot(RIORef,'IOExts.IORef'(share(MutVal))),
        get_mutable(Val,MutVal),
	(Val='$eval'(V) -> true
	  ; create_mutable(Val,MutV),
	    update_mutable(share(MutV),MutVal),
	    V=share(MutV)),
	E0=E.

% Assign a new value to an IORef:
?- block prim_writeIORef(?,?,?,-,?).
prim_writeIORef(R,V,partcall(1,exec_writeIORef,[V,R]),E,E).

?- block exec_writeIORef(?,?,?,?,-,?).
exec_writeIORef(RIORef,Val,_,R,E0,E) :-
        user:derefRoot(RIORef,IORef),
	prim_writeIORef_exec(IORef,Val,R), E0=E.

prim_writeIORef_exec('IOExts.IORef'(share(MutVal)),Val,'$io'('Prelude.()')) :-
        var(Val), !,
	update_mutable('$eval'(Val),MutVal).
prim_writeIORef_exec('IOExts.IORef'(share(MutVal)),Val,'$io'('Prelude.()')) :-
	update_mutable(Val,MutVal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
