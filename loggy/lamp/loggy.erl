%%%-------------------------------------------------------------------
%%% @author gabrielemorello
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Sep 2022 13:20
%%%-------------------------------------------------------------------
-module(loggy).
-author("gabrielemorello").

%% API
-export([start/1, stop/1, loop/2]).
-import(vect,[safe/1]).
start(Nodes) ->
  spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
  Logger ! stop.

init(Nodes) ->
  loop(time:clock(Nodes), []).

loop(Clock, Queue) ->
  receive
    {log, From, Time, Msg} ->
      %io:format("Recv: ~w ~w ~p~n", [Time, From, Msg]),

      NewClock = time:update(From, Time, Clock),
      %io:format("Clocks: ~w~n~w~n", [Clock, NewClock]),

      NewQueue1 = lists:keysort(2, [{From, Time, Msg} | Queue]),

      NewQueue2 = rec_check(NewQueue1, [], NewClock),
      %io:format("Queue: ~w~n~w~n~w~n", [Queue, NewQueue1, NewQueue2]),

      loop(NewClock, NewQueue2);
    stop ->
      io:format("~n Holdback Queue: ~w~n", [Queue]),
      ok
  end.

rec_check([], Acc, _) ->
  Acc;
rec_check([{From, Time, Msg} | Rest], Acc, Clock) ->
  case time:safe(Time, Clock) of
    true ->
      log(From, Time, Msg, Clock),
      rec_check(Rest, Acc, Clock);
    false ->
      rec_check(Rest, [{From, Time, Msg} | Acc], Clock)
  end.

log(From, Time, Msg, Clock) ->
  io:format("log: ~w ~w ~p ~w ~n", [Time, From, Msg, Clock]).

