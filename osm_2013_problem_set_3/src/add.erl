%% @doc Erlang mini project.
-module(add).
-export([start/3, start/4, adder/4, manager/5,calculate/6,encode/2 ]).

-include_lib("eunit/include/eunit.hrl").

%% @doc Calculates A+B using only one process and returns the desired value in base Base.
-spec start(A,B,Base) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer().
start(A,B, Base) ->
    start(A,B,Base,1).
%% @doc Calculates A+B using Options processes and returns the desired value in base Base.
-spec start(A,B,Base, Options) -> ok when 
      A::string(),
      B::string(), 
      Base::integer(),
      Options::integer().

start(A,B,Base, Options) ->
    Maxlen = max(length(A),length(B)),
    Opts = min(Maxlen,Options),
    Acorr = fill(A,Maxlen),
    Bcorr = fill(B,Maxlen),
    Asplit =utils:split(Acorr,Opts),
    Bsplit = utils:split(Bcorr,Opts),
    Segsize = Maxlen div Opts,
    {Clist, Rlist} = dispatch(Asplit,Bsplit,Base,self()),
    print(lists:map(fun(X) -> X+48 end,Clist),""),
    print(Acorr," "),
    print(Bcorr," "),
    print(generate("-",length(Rlist)+1),""),
    if length(Rlist) =:= length(Acorr) ->
	    Roff = " ";
       true -> 
	    Roff = ""
    end,
    print(Rlist,Roff).

%% @doc Splits A and B into blocks of size Segsize and creates processes for each block that calculates the results.
-spec dispatch(A,B,Base,PID) -> ok when 
      A::list(),
      B::list(),
      Base::integer(),
      PID::pid().
dispatch([],[],_,PID) ->
    PID ! {[0],[]},
    receive
	{[C|Clist], Rlist} ->
	    if C =:= 1 ->
		    {[C|Clist],[49|Rlist]};
	       C =:= 0 -> 
		    {[C|Clist], Rlist}
	    end
    end;
dispatch([Ah|At],[Bh|Bt],Base,PID) ->
    NewPid = spawn(?MODULE, adder, [Ah,Bh,Base,PID]),
    dispatch(At,Bt,Base,NewPid).

%%dispatch(A,B,Segsize,Base,PID)  -> 
 %%   {Ahead, Atail} = split(A,Segsize),
   %%% {Bhead, Btail} = split(B,Segsize),
 %%   NewPid = spawn(?MODULE,adder,[Ahead,Bhead,Base,PID]),
   %% dispatch(Atail,Btail,Segsize,Base,NewPid).


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
	    {Cfix,_} = lists:split(length(C)-1,C),
	    Aout = lists:append(Acc,Ain),
	    Cout = lists:append(Cfix,Cin),
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
    fillAux(N-1,[48|List]).

%% @doc Prints the list List.
-spec print(List,Offset) -> ok when 
      List::list(),
      Offset::string().
print(List, Offset) ->
    io:fwrite(intlist_to_string(List,Offset)),%%lists:flatten(Nested)).
    io:fwrite("~n").

intlist_to_string(L, Offset) ->
    AtList = lists:map(fun(X) ->
		      io_lib:write_atom(binary_to_term(<<131,100,0,1 ,X>>)) end,
	      L),
    UglyList = lists:foldr(fun(Char,Str) ->
				   Char++Str end,"",AtList),
    Offset++lists:filter(fun(X) ->
		   X =/= 39 end, UglyList).
    
		      

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
    genAux(Str,N-1,Str++A).

%% @doc Converts the first character into a decimal number.
-spec decode([H|_]) -> ok when 
      H::integer().
decode(H) when H >=48, H =<57->
    H - 48;
decode(H) when H >=65, H =<90 ->
    H -55;
decode(H) ->
    io:fwrite("~w", [H]),
    0.



%% @doc Converts a decimal number into base Base.
-spec encode(Val,Base) -> ok when 
      Val::integer(),
      Base::integer().
encode(Val,Base) ->
    Q = Val div Base,
    Rem = Val rem Base,
    %%Rstr = io:format("~s",[[Rem]]),
    if Rem <10 ->
	    Rstr = Rem+48;
       Rem >=10 -> 
	    Rstr = Rem+55
    end,
    {Q,Rstr}.
    
    

% ---------------------- TEST -------------------

decode_test_() ->
    [?_assertEqual(17,decode(72)),
    ?_assertEqual(2, decode(50))].
encode_test_() ->
    [?_assertEqual({0,72},encode(17,26)),
    ?_assertEqual({1,49},encode(11,10))].
generate_test_() ->
    [?_assertEqual("AAAAA",generate("A",5)),
    ?_assertEqual("-.--.--.--.-",generate("-.-",4))].
split_test_() ->
    [?_assertEqual({[1,2],[3,4,5,6,7,8]},split([1,2,3,4,5,6,7,8],2)),
    ?_assertEqual({[1,2,3,4,5,6,7,8],[]},split([1,2,3,4,5,6,7,8],9))].
fill_test_() ->
    [?_assertEqual([48,48,49,50,51,52,53],fill([49,50,51,52,53],7)),
    ?_assertEqual([49,50,51,52,53],fill([49,50,51,52,53],5))].
dispatch_test_() ->
    [?_assertEqual({[0,0,0,0,0],[52,52,50,56]},dispatch([[50,50],[49,52]],[[50,50],[49,52]],10,self()))].

