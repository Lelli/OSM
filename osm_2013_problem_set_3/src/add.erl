%% @doc Erlang mini project.
-module(add).
-export([start/3, start/4]).

%% @doc TODO: add documentation
-spec start(A,B,Base) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer().

start(A,B, Base) ->
 
    ok.
%% @doc TODO: add documentation
%%-spec start(A,B,Base, Options) -> ok when 
 %%     A::string(),
  %%    B::string(), 
   %%   Base::integer(),
    %%  Option::integer().
%%      Options::[Option].

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


fill(List,Limit) ->
    N = Limit - length(List),
    fillAux(N,List).

fillAux(0,List) ->
    List;
fillAux(N,List) ->
    fillAux(N-1,[0|List]).

print(List) ->
    io:write("~n",List).

split(List,Segsize) ->
    if length(List) =< Segsize ->
	    {List,[]};
       length(List) > Segsize ->
	    lists:split(Segsize,List)
    end.

generate(Str,N) ->
    genAux(Str,N,[]).
genAux(Str,0,A) ->
    A;
genAux(Str,N,A) ->
    genAux(Str,N-1,[Str|A]).

decode([H|_]) ->
    Ascii = H,
    if Ascii >= 48; Ascii =< 57
       -> Val = Ascii - 48;
       Ascii >= 65; Ascii =< 90
       -> Val = Ascii - 55
    end.

encode(Val,Base) ->
    Q = Val div Base,
    Rem = Val rem Base,
    Rstr = io:format("~s",[[Rem]]),
    {Q,Rstr}.
    
    

% ---------------------- TEST -------------------

