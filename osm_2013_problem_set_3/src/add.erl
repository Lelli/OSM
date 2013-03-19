%% @doc Erlang mini project.
-module(add).
-export([start/3, start/4]).

%% @doc Calculates A+B using only one process and returns the desired value in base Base.
-spec start(A,B,Base) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer().
start(A,B, Base) ->
    ok.
%% @doc Calculates A+B using Options processes and returns the desired value in base Base.
-spec start(A,B,Base, Options) -> ok when 
      A::string(),
      B::string(), 
      Base::integer(),
      Options::integer().

start(A,B,Base, Options) ->
    Maxlen = max(length(A),length(B)),
    Acorr = fill(A,Maxlen),
    Bcorr = fill(B,Maxlen),
    Segsize = Maxlen div Options,
    {Clist, Rlist} = dispatch(Acorr,Bcorr,Segsize,Base,self()),
    print(Clist),
    print(Acorr),
    print(Bcorr),
    print(generate("-",Maxlen)),
    print(Rlist).

%% @doc Splits A and B into blocks of size Segsize and creates processes for each block that calculates the results.
-spec dispatch(A,B,Segsize,Base,PID) -> ok when 
      A::list(),
      B::list(),
      Segsize::integer(),
      Base::integer(),
      PID::pid().
dispatch([],[],_,_,PID) ->
    PID ! {[0],[]},
    receive
	{[C|Clist], Rlist} ->
	    if C =:= 1 ->
		    {[C|Clist],[C|Rlist]};
	       C =:= 0 -> 
		    {[C|Clist], Rlist}
	    end
    end;
dispatch(A,B,Segsize,Base,PID)  -> 
    {Ahead, Atail} = split(A,Segsize),
    {Bhead, Btail} = split(B,Segsize),
    NewPid = spawn(?MODULE,adder,[Ahead,Bhead,Base,PID]),
    dispatch(Atail,Btail,Segsize,Base,NewPid).

%% @doc Revereses the lists A and B and spawns two processes, one that counts with a carry bit and one that counts without a carry bit.
-spec adder(A,B,Base,PID) -> ok when 
      A::list(),
      B::list(),
      Base::integer(),
      PID::pid().
adder(A,B,Base,PID) ->
    Arev = lists:reverse(A),
    Brev = lists:reverse(B),
    Man0 = spawn(?MODULE,manager,[Arev,Brev,Base,0,PID]),
    Man1 = spawn(?MODULE,manager,[Arev,Brev,Base,1,PID]),
    receive
	{C, Rlist} ->
	    Man0 ! {C,Rlist},  
	    Man1 ! {C,Rlist}
    end.

%% @doc Checks if we have a carry bit or not and sends this information down to the child so that it can decide what to do.
-spec manager(A,B,Base,C,PID) -> ok when 
      A::list(),
      B::list(),
      C::list(),
      Base::integer(),
      PID::pid().
manager(A,B,Base,C,PID) ->
    Child = spawn(?MODULE,calculate,[A,B,Base,[C|[]],PID,[]]),
    receive
	{[CarryIn|Clist],Rlist}->
	    if CarryIn=:= C ->
		    Child ! {[CarryIn|Clist],Rlist};
	       CarryIn =/= C->
		    exit(Child,wrongCarry)
	    end
    end.

%% @doc Calculates the result of A and B and stores it in Acc.
-spec calculate(A,B,Base,C,PID,Acc) -> ok when 
      A::list(),
      B::list(),
      C::list(),
      Base::integer(),
      PID::pid(),
      Acc::list().
calculate([],[],_,C,Pid,Acc) ->
    receive {Cin,Ain} ->
	    Aout = lists:append(Acc,Ain),
	    Cout = lists:append(C,Cin),
	    Pid ! {Cout,Aout}
    end;
  

calculate([A|Alist],[B|Blist],Base,[C|Clist],PID,Acc) ->
    Adec = decode(A),
    Bdec = decode(B),
    Result = Adec + Bdec + C,
    {Cnew, Rnew} = encode(Result,Base),
    calculate(Alist,Blist,Base,[Cnew|[C|Clist]],PID,[Rnew|Acc]).

%% @doc Adds zeros to the head of List until the length of List =:= Limit.
-spec fill(List,Limit) -> ok when 
      List::list(),
      Limit::integer().

fill(List,Limit) ->
    N = Limit - length(List),
    fillAux(N,List).

%% @doc Help function to fill.
-spec fillAux(N,List) -> ok when 
      List::list(),
      N::integer().
fillAux(0,List) ->
    List;
fillAux(N,List) ->
    fillAux(N-1,[0|List]).

%% @doc Prints the list List.
-spec print(List) -> ok when 
      List::list().
print(List) ->
    io:write("~n",List).

%% @doc Splits the list into two lists, the length of the first list is equal to Segsize.
-spec split(List,Segsize) -> ok when 
      List::list(),
      Segsize::integer().
split(List,Segsize) ->
    if length(List) =< Segsize ->
	    {List,[]};
       length(List) > Segsize ->
	    lists:split(Segsize,List)
    end.

%% @doc Generates a string of characters depending on the length N.
-spec generate(Str,N) -> ok when 
      Str::list(),
      N::integer().
generate(Str,N) ->
    genAux(Str,N,[]).
%% @doc Help function to generate.
-spec genAux(Str,N,A) -> ok when 
      Str::list(),
      N::integer(),
      A::list().
genAux(_,0,A) ->
    A;
genAux(Str,N,A) ->
    genAux(Str,N-1,[Str|A]).

%% @doc Converts the first character into a decimal number.
-spec decode([H|_]) -> ok when 
      H::integer().
decode([H|_]) ->
    Ascii = H,
    if Ascii >= 48; Ascii =< 57
       ->  Ascii - 48;
       Ascii >= 65; Ascii =< 90
       ->  Ascii - 55
    end.

%% @doc Converts a decimal number into base Base.
-spec encode(Val,Base) -> ok when 
      Val::integer(),
      Base::integer().
encode(Val,Base) ->
    Q = Val div Base,
    Rem = Val rem Base,
    Rstr = io:format("~s",[[Rem]]),
    {Q,Rstr}.
    
    

% ---------------------- TEST -------------------

