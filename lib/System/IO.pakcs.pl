%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module System.IO
%
% Note: the Prolog term '$stream'('$inoutstream'(In,Out)) represents a handle
% for a stream that is both readable (on In) and writable (on Out)
% Otherwise, handles are represented by Prolog terms of the form '$stream'(N).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% equality of two handles:
'System.IO.handle_eq'(H1,H2,B) :-
        (H1=H2 -> B='Prelude.True' ; B='Prelude.False').

'System.IO.stdin'(Stream) :- stdInputStream(Stream).

'System.IO.stdout'(Stream) :- stdOutputStream(Stream).

'System.IO.stderr'(Stream) :- stdErrorStream(Stream).

'System.IO.prim_openFile'(A,Mode,Stream) :-
	string2Atom(A,FName),
	curryFileMode2plmode(Mode,PMode),
	fileOpenOptions(Options),
	open(FName,PMode,Stream,Options).

curryFileMode2plmode('System.IO.ReadMode',read).
curryFileMode2plmode('System.IO.WriteMode',write).
curryFileMode2plmode('System.IO.AppendMode',append).


'System.IO.prim_hClose'('$stream'('$inoutstream'(In,Out)),'Prelude.()') :- !,
	flush_output(Out),
	close(Out),
	(In==Out -> true ; close(In)).
'System.IO.prim_hClose'(Stream,'Prelude.()') :-
	(isOutputStream(Stream) -> flush_output(Stream) ; true),
	close(Stream).


'System.IO.prim_hFlush'('$stream'('$inoutstream'(_,Out)),'Prelude.()') :- !,
	flush_output(Out).
'System.IO.prim_hFlush'(Stream,'Prelude.()') :-
	(isOutputStream(Stream) -> flush_output(Stream) ; true).


'System.IO.prim_hIsEOF'('$stream'('$inoutstream'(In,_)),B) :- !,
	(atEndOfStream(In) -> B='Prelude.True' ; B='Prelude.False').
'System.IO.prim_hIsEOF'(Stream,B) :-
	(atEndOfStream(Stream) -> B='Prelude.True' ; B='Prelude.False').


'System.IO.prim_hSeek'(Handle,SeekMode,Pos,'Prelude.()') :-
	currySeekMode2plmode(SeekMode,PlSM),
	seek(Handle,Pos,PlSM,_).

currySeekMode2plmode('System.IO.AbsoluteSeek',bof).
currySeekMode2plmode('System.IO.RelativeSeek',current).
currySeekMode2plmode('System.IO.SeekFromEnd',eof).


?- block prim_hWaitForInput(?,?,?,-,?).
prim_hWaitForInput(Hdl,TO,partcall(1,exec_hWaitForInput,[TO,Hdl]),E,E).
?- block exec_hWaitForInput(?,?,?,?,-,?).
exec_hWaitForInput(RStream,RTO,World,'$io'(B),E0,E) :-
	exec_hWaitForInputs([RStream],RTO,World,'$io'(N),E0,E1),
	(N=0 -> B='Prelude.True' ; B='Prelude.False'),
	!, E1=E.

?- block prim_hWaitForInputs(?,?,?,-,?).
prim_hWaitForInputs(H,T,partcall(1,exec_hWaitForInputs,[T,H]),E,E).
?- block exec_hWaitForInputs(?,?,?,?,-,?).
exec_hWaitForInputs(RStreams,RTO,_,'$io'(N),E0,E) :-
	user:derefAll(RStreams,Streams),
	selectInstreams(Streams,InStreams),
	user:derefRoot(RTO,TimeOut),
	waitForInputDataOnStreams(InStreams,TimeOut,N),
	!, E0=E.

selectInstreams([],[]).
selectInstreams(['$stream'('$inoutstream'(In,_))|Streams],[In|InStreams]) :- !,
	selectInstreams(Streams,InStreams).
selectInstreams([Stream|Streams],[Stream|InStreams]) :-
	selectInstreams(Streams,InStreams).


'System.IO.prim_hGetChar'('$stream'('$inoutstream'(In,_)),C) :- !,
	get_code(In,N), char_int(C,N).
'System.IO.prim_hGetChar'(Stream,C) :-
	get_code(Stream,N), char_int(C,N).


'System.IO.prim_hPutChar'('$stream'('$inoutstream'(_,Out)),C,'Prelude.()') :- !,
	char_int(C,N), put_code(Out,N).
'System.IO.prim_hPutChar'(Stream,C,'Prelude.()') :-
	char_int(C,N), put_code(Stream,N).


'System.IO.prim_hIsReadable'('$stream'('$inoutstream'(_,_)),'Prelude.True') :-
        !.
'System.IO.prim_hIsReadable'(Stream,B) :-
	(isInputStream(Stream) -> B='Prelude.True' ; B='Prelude.False').


'System.IO.prim_hIsWritable'('$stream'('$inoutstream'(_,_)),'Prelude.True') :-
        !.
'System.IO.prim_hIsWritable'(Stream,B) :-
	(isOutputStream(Stream) -> B='Prelude.True' ; B='Prelude.False').


'System.IO.prim_hIsTerminalDevice'('$stream'('$inoutstream'(_,S)),R) :- !,
	prim_hIsTerminalDevice(S,R).
'System.IO.prim_hIsTerminalDevice'(Stream,B) :-
	(isTerminalDeviceStream(Stream) -> B='Prelude.True'
	                                 ; B='Prelude.False').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% choice on a stream and an external message stream:
?- block prim_hWaitForInputsOrMsg(?,?,?,-,?).
prim_hWaitForInputsOrMsg(H,M,partcall(1,exec_hWaitForInputsOrMsg,[M,H]),E,E).

?- block exec_hWaitForInputsOrMsg(?,-,?,?,?,?),
         exec_hWaitForInputsOrMsg(?,?,?,?,-,?).
exec_hWaitForInputsOrMsg(Handles,share(M),World,Result,E0,E) :-
        !,
	get_mutable(V,M),
	(V='$eval'(R) % external message stream has been already evaluated
	 -> E0=E, Result='$io'('Prelude.Right'(R))
	  ; exec_hWaitForInputsOrMsg(Handles,V,World,CResult,E0,E),
	    (CResult='$io'('Prelude.Left'(_))
  	     -> Result=CResult
	      ; CResult='$io'('Prelude.Right'(S)),
	        (compileWithSharing(variable) -> user:propagateShare(S,TResult)
		                               ; S=TResult),
	        Result='$io'('Prelude.Right'(TResult)),
	        update_mutable('$eval'(TResult),M))).
exec_hWaitForInputsOrMsg(_,[M|Ms],_,'$io'('Prelude.Right'([M|Ms])),E0,E) :-
	!, E0=E. % stream already evaluated
exec_hWaitForInputsOrMsg(RHandles,[],_,'$io'('Prelude.Left'(N)),E0,E) :- !,
	% message stream is empty, so anything must be received from the handles.
	user:derefAll(RHandles,Handles),
	selectInstreams(Handles,InStreams),
	waitForInputDataOnStreams(InStreams,-1,N),
	!, E0=E.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
