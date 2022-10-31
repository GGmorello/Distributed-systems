%%%-------------------------------------------------------------------
%%% @author gabrielemorello
%%% @copyright (C) 2022, <COMPANY>
%%% @doc

%%% @end
%%% Created : 08. Sep 2022 16:39
%%%-------------------------------------------------------------------
-module(rudy).
-author("gabrielemorello").

%% API
-export([start/2, stop/0]).
-import(http, [parse_request/1, ok/1]).


init(Port, N) ->
  Opt = [list, {active, false}, {reuseaddr, true}, {m, 20}],
  case gen_tcp:listen(Port, Opt) of
    {ok, Listen} ->
      handlers(Listen, N),
      super(Listen);
    {error, Error} ->
        io:format("rudy: init error: ~w~n", [Error]),
        error
  end.

handler(Listen, N) ->
  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      request(Client),
      gen_tcp:close(Client),
      handler(Listen, N);
    {error, Error} ->
      io:format("rudy: handler error: ~w~n", [Error]),
      error
  end.

request(Client) ->
  Recv = gen_tcp:recv(Client, 0),
  case Recv of
    {ok, Str} ->
      Request = parse_request(Str),
      Response = reply(Request),
      gen_tcp:send(Client, Response);
    {error, Error} ->
      io:format("rudy: request error: ~w~n", [Error]),
      error
  end,
  gen_tcp:close(Client).

reply({{get, _, _}, _, Body}) ->
  factorization:divlist(99194853094755497),
  %timer:sleep(40),
  http:ok(Body).

start(Port, N) ->
  register(rudy, spawn(fun() -> init(Port, N) end)).

stop() ->
  rudy ! stop.

handlers(Listen, N) ->
  case N of
    0 ->
      ok;
    N ->
      spawn(fun()->handler(Listen, N) end),
      handlers(Listen, N-1)
  end.

super(Listen) ->
  receive
    stop ->
      gen_tcp:close(Listen),
      ok
  end.

