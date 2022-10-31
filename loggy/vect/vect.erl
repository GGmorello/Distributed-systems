%%%-------------------------------------------------------------------
%%% @author gabrielemorello
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Sep 2022 14:04
%%%-------------------------------------------------------------------
-module(vect).
-author("gabrielemorello").

%% API
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() ->
  [].

inc(Name, Time) ->
  case lists:keyfind(Name, 1, Time) of
    {Name, T} ->
      lists:keyreplace(Name, 1, Time, {Name, T+1});
    false ->
      [{Name, 1}]
  end.

merge([], Time) ->
  Time;
merge([{Name, Ti}|Rest], Time) ->
  case lists:keyfind(Name, 1, Time) of
    {Name, Tj} ->
      [{Name, max(Ti,Tj)} | merge(Rest, lists:keydelete(Name, 1, Time))];
    false ->
      [{Name, Ti} | merge(Rest, Time)]
  end.

leq([], _) ->
  true;
leq([{Name, Ti}|Rest],Time) ->
  case lists:keyfind(Name, 1, Time) of
    {Name, Tj} ->
      if
        Ti =< Tj ->
          leq(Rest, Time);
        true ->
          false
      end;
    false ->
      false
  end.

clock(Nodes) ->
  [].

update(From, Time, Clock) ->
  case lists:keyfind(From, 1, Clock) of
    {From, _} ->
      lists:keyreplace(From, 1, Clock, {From, Time});
    false ->
      [{From, Time} | Clock]
  end.

safe(Time, Clock) ->
  lists:all(fun({_, TC}) -> leq(Time, TC) end, Clock).