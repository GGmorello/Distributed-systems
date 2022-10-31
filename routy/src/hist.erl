%%%-------------------------------------------------------------------
%%% @author gabrielemorello
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Sep 2022 10:28
%%%-------------------------------------------------------------------
-module(hist).
-author("gabrielemorello").

%% API
-export([new/1, update/3]).


new(Name) ->
  [{Name, inf}].

update(Node, N, History) ->
  case lists:keyfind(Node, 1, History) of
    false ->
      {new, [{Node, N} | History]};
    {_, NF} ->
      case N > NF of
        true ->
          H1 = lists:keydelete(Node, 1, History),
          {new, [{Node, N} | H1]};
        false->
          old
      end
  end.