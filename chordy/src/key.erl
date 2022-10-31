%%%-------------------------------------------------------------------
%%% @author gabrielemorello
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2022 14:58
%%%-------------------------------------------------------------------
-module(key).
-author("gabrielemorello").

%% API
-export([generate/0, between/3]).

generate() ->
  rand:uniform(1000000000).

between(Key, From, To) ->
  if
    (From < To) and (Key > From) and (Key =< To) ->
      true;
    (From > To) and ((Key > From) or (Key =< To)) ->
      true;
    From == To ->
      true;
    true ->
      false
  end.