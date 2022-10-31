%%%-------------------------------------------------------------------
%%% @author gabrielemorello
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2022 16:59
%%%-------------------------------------------------------------------
-module(storage).
-author("gabrielemorello").

%% API
-compile(export_all).

create() ->
  [].

add(Key, Value, Store) ->
  [{Key, Value} | Store].

lookup(Key, Store) ->
  lists:keyfind(Key, 1, Store).

split(From, To, Store) ->
  lists:partition(fun({Key,_})-> key:between(Key, From, To) end, Store).

merge(Entries, Store) ->
  lists:append(Store, Entries).
