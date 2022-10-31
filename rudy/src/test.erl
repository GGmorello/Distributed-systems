%%%-------------------------------------------------------------------
%%% @author gabrielemorello
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Sep 2022 01:44
%%%-------------------------------------------------------------------
-module(test).
-author("gabrielemorello").

%% API
-export([init/2]).

init(N, P)->
  case P of
    0 ->
      ok;
    P ->
      spawn(fun() -> bench(localhost, 8080, N) end),
      init(N, P-1)
  end.

bench(Host, Port, N) ->
  Start = erlang:system_time(micro_seconds),
  run(N, Host, Port),
  Finish = erlang:system_time(micro_seconds),
  io:format("~w~n", [Finish-Start]).

run(N, Host, Port) ->
  if
    N == 0 ->
      ok;
    true ->
      request(Host, Port),
      run(N-1, Host, Port)
  end.

request(Host, Port) ->
  % Opt = [list, {active, false}, {reuseaddr, true}, {packet, 2}],
  Opt = [list, {active, false}, {reuseaddr, true}],
  {ok, Server} = gen_tcp:connect(Host, Port, Opt),
  % Str = gen_str(100000),
  % gen_tcp:send(Server, http:get("lolol", Str)),
  gen_tcp:send(Server, http:get("foo")),
  Recv = gen_tcp:recv(Server, 0),
  case Recv of
    {ok, _} ->
      ok;
    {error, Error} ->
      io:format("test: error: ~w~n", [Error])
  end,
  gen_tcp:close(Server).

-ifdef(Comment).
gen_str(N)->
  binary_to_list(crypto:strong_rand_bytes(N)).
-endif.