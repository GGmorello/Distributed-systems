%%%-------------------------------------------------------------------
%%% @author gabrielemorello
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Sep 2022 23:01
%%%-------------------------------------------------------------------
-module(intf).
-author("gabrielemorello").

%% API
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).


new() ->
  [].

add(Name, Ref, Pid, Intf) ->
  case lists:member({Name, Ref, Pid}, Intf) of
    true ->
      Intf;
    false ->
      [{Name, Ref, Pid} | Intf]
  end.

remove(Name, Intf) ->
  lists:keydelete(Name, 1, Intf).

lookup(Name, Intf) ->
  case lists:keyfind(Name, 1, Intf) of
    false ->
      notfound;
    {Name,_,Pid} ->
      {ok, Pid}
  end.


ref(Name, Intf) ->
  case lists:keyfind(Name, 1, Intf) of
    false ->
      notfound;
    {Name, Ref,_} ->
      {ok, Ref}
  end.

name(Ref, Intf) ->
  case lists:keyfind(Ref, 2, Intf) of
    false ->
      notfound;
    {Name, Ref,_} ->
      {ok, Name}
  end.

list(Intf) ->
  list(Intf, []).
list(Intf, Final) ->
  case Intf of
    [] ->
      Final;
    [{Name, _, _} | T] ->
      list(T, [Name | Final])
  end.

broadcast(Message, Intf) ->
  lists:foldl(fun({_,_,Pid}, _) -> Pid ! Message end, [], Intf).