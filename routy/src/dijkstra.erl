%%%-------------------------------------------------------------------
%%% @author gabrielemorello
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Sep 2022 01:00
%%%-------------------------------------------------------------------
-module(dijkstra).
-author("gabrielemorello").

%% API
-export([table/2, route/2, iterate/3]).

entry(Node, Sorted) ->
  Entry = lists:keyfind(Node, 1, Sorted),
  case Entry of
    false ->
      0;
    Entry->
      {_, Length, _} = Entry,
      Length
  end.

replace(Node, N, Gateway, Sorted) ->
  Sorted1 = lists:keydelete(Node, 1, Sorted),
  Sorted2 = [{Node, N, Gateway} | Sorted1],
  lists:sort(fun({_, A, _}, {_, B, _}) -> A =< B end, Sorted2).

update(Node, N, Gateway, Sorted) ->
  L = entry(Node, Sorted),
  if
    N < L ->
      replace(Node, N, Gateway, Sorted);
    true ->
      Sorted
  end.

iterate([], _, Table) ->
  Table;
iterate([{_,inf,_}|_], _, Table) ->
  Table;
iterate([{Node, N, Gateway} | Nodes], Map, Table) ->
  Table1  = [{Node, Gateway}|Table],
  case map:reachable(Node, Map) of
    [] ->
      iterate(Nodes, Map, Table1);
    Links ->
      Sorted1 = lists:foldl(fun(Link, Rest2) -> update(Link, N+1, Gateway, Rest2) end, Nodes, Links),
      iterate(Sorted1, Map, Table1)
  end.

table(Gateways, Map) ->
  AllNodes = map:all_nodes(Map),
  List = lists:foldl(fun(Node, R) -> [{Node, inf, unknown} | R] end, [], AllNodes),
  List2 = lists:foldl(fun(Gateway, R) -> replace(Gateway, 0, Gateway, R) end, List, Gateways),
  iterate(List2, Map, []).


route(Node, Table) ->
  case lists:keyfind(Node, 1, Table) of
    false ->
      notfound;
    {Node, Gateway} ->
      {ok, Gateway}
  end.
