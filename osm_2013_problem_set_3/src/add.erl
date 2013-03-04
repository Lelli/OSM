%% @doc Erlang mini project.
-module(add).
-export([start/3, start/4]).

%% @doc TODO: add documentation
-spec start(A,B,Base) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer().

start(A,B, Base) ->
    Ab = utils:decToBase(A,Base),
    Bb = utils:decToBase(B,Base),
    {Clist,Rlist} = start_help(Ab,Bb,Base,[],[]),
    utils:printList(Clist),
    utils:line(length(Clist)),
    io:fwrite(" ~p~n",[A]),
    io:fwrite(" ~p~n",[B]),
    utils:line(length(Clist)),
    utils:printList(Rlist),
    ok.
%% @doc TODO: add documentation
-spec start(A,B,Base, Options) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer(),
      Option::atom() | tuple(),
      Options::[Option].

start(A,B,Base, Options) ->
    ok.


start_help(0,0,_,[H|T],Result) ->
    if 
	H =:= 1 ->
	    {[H|T],[1|Result]};
	true ->
	    {[H|T],Result}
    end;
	
%%start_help(0,B,Base,[H|T],Result) -> 
%%    if 
%%	H+B =:= Base->
%%	    {[1|[H|T]],[1|Result]};
%%	true ->
%%	    {[H|T],Result}
  %%  end;
%%start_help(A,0,Base,[H|T],Result) -> 
%%    if 
%%	H+A =:= Base ->
%%	    {[1|[H|T]],[1|Result]};
%%	true -> 
%%	    {[H|T],Result}
  %%  end;
	
start_help(A,B,Base,[H|T],Result) -> 
    Amod = A rem Base,
    Bmod = B rem Base,
    if 
	Amod+Bmod+H >= Base ->
	    start_help(A div Base, B div Base, Base,[1|[H|T]],
		       [((H+Amod+Bmod)rem Base)|Result]);
        true ->
	    start_help(A div Base, B div Base, Base,[0|[H|T]],
		       [((H+Amod+Bmod)rem Base )|Result])
    
    end;
	

start_help(A,B,Base,[],[]) -> 
  Amod = A rem Base,
    Bmod = B rem Base,
    if 
	Amod+Bmod >= Base ->
	    start_help(A div Base, B div Base, Base,[1],
		       [((Amod+Bmod)rem Base)]);
        true ->
	    start_help(A div Base, B div Base, Base,[0],
		       [((Amod+Bmod)rem Base )])
    
    end.
	

% ---------------------- TEST -------------------

