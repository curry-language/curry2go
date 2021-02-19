%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module System.Environment
%

'System.Environment.getArgs'(StringArgs) :-
        (rtArgs(Args) -> true ; getProgramArgs(Args)),
        map2M(basics:atom2String,Args,StringArgs).

'System.Environment.prim_getEnviron'(Var,Value) :-
	string2Atom(Var,AtomVar),
	(getEnv(AtomVar,AtomValue) -> atom2String(AtomValue,Value)
	                            ; Value = []). % empty string if undefined

'System.Environment.prim_setEnviron'(Var,Value,'Prelude.()') :-
	string2Atom(Var,AtomVar),
	string2Atom(Value,AtomValue),
	setEnv(AtomVar,AtomValue).

'System.Environment.prim_unsetEnviron'(Var,'Prelude.()') :-
	string2Atom(Var,AtomVar),
	unsetEnv(AtomVar).

'System.Environment.getHostname'(String) :-
        getHostname(Name),
        atom2String(Name,String).

'System.Environment.getProgName'(String) :-
        user:currentModuleFile(Name,_),
        atom2String(Name,String).

'System.Environment.isWindows'(Flag) :-
	getEnv('COMSPEC', _) ->
	  % Windows systems define this environment variable...
	  Flag = 'Prelude.True'
	; Flag = 'Prelude.False'.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
