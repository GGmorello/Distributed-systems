%%%-------------------------------------------------------------------
%%% @author gabrielemorello
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Sep 2022 14:04
%%%-------------------------------------------------------------------
-module(time).
-author("gabrielemorello").

%% API
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() ->
  0.

inc(Name, Time) -> Time + 1.

merge(Ti, Tj) -> lists:max([Ti, Tj]).

leq(Ti, Tj) -> Ti =< Tj.

clock(Nodes) ->
  lists:foldl(fun(El, Acc) -> [{El, zero()} | Acc] end, [], Nodes).

update(From, Time, Clock) ->
  Clock1 = lists:keyreplace(From, 1, Clock, {From, Time}),
  lists:keysort(2, Clock1).

safe(Time, [{_, OldTime}| _ ]) ->
  leq(Time, OldTime).