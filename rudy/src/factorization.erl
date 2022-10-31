%%%-------------------------------------------------------------------
%%% @author gabrielemorello
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Sep 2022 08:51
%%%-------------------------------------------------------------------
-module(factorization).
-author("gabrielemorello").

%% API
-export([divlist/1]).

divlist(N) ->
  automult([1|decomp(N)]).

decomp(N) when is_integer(N), (N > 0) ->
  lists:reverse(decomp(N,[],2)).

decomp(N,R,I) when I*I > N -> [N|R];
decomp(N,R,I) when (N rem I) =:= 0 -> decomp(N div I,[I|R],I);
decomp(N,R,2) ->
  decomp(N,R,3);
decomp(N,R,I) ->
  decomp(N,R,I+2).

automult(L=[H]) when is_number(H)-> L;
automult([H|Q]) when is_number(H)->
  L1 = automult(Q),
  L2 = [H*X || X <- L1],
  lists:usort([H|L1]++L2).