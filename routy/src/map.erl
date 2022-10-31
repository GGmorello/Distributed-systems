%%%-------------------------------------------------------------------
%%% @author gabrielemorello
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Sep 2022 17:31
%%%-------------------------------------------------------------------
-module(map).
-author("gabrielemorello").

%% API
-export([new/0, update/3, reachable/2, all_nodes/1]).


new()->
  [].

update(Node, Links, Map) ->
  Map1 = lists:keydelete(Node, 1, Map),
  [{Node, Links} | Map1].

reachable(Node, Map) ->
  case lists:keyfind(Node, 1, Map) of
    {_, Reach} ->
      Reach;
    false ->
      []
  end.


all_nodes(Map) ->
  {A, B} = lists:unzip(Map),
  C = lists:merge(B),
  D = lists:append(A, C),
  lists:uniq(D).
