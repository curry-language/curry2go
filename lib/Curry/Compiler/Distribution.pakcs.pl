%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module Distribution
%

'Curry.Compiler.Distribution.curryCompiler'(CS) :- atom2String(pakcs,CS).

'Curry.Compiler.Distribution.curryCompilerMajorVersion'(V) :-
        compilerMajorVersion(V).

'Curry.Compiler.Distribution.curryCompilerMinorVersion'(V) :-
        compilerMinorVersion(V).

'Curry.Compiler.Distribution.curryCompilerRevisionVersion'(V) :-
        compilerRevisionVersion(V).

'Curry.Compiler.Distribution.curryRuntime'(PrologS) :-
        prolog(Prolog), atom2String(Prolog,PrologS).

'Curry.Compiler.Distribution.curryRuntimeMajorVersion'(V) :-
        prologMajorVersion(V).

'Curry.Compiler.Distribution.curryRuntimeMinorVersion'(V) :-
        prologMinorVersion(V).

'Curry.Compiler.Distribution.baseVersion'(BVS) :-
        baseVersion(BVA), atom2String(BVA,BVS).

'Curry.Compiler.Distribution.installDir'(PHS) :-
	installDir(PH)
	 -> atom2String(PH,PHS)
	  ; raise_exception('Curry.Compiler.Distribution.installDir: cannot determine installation directory!').
