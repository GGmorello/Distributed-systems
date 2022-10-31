%%%-------------------------------------------------------------------
%%% @author gabrielemorello
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Sep 2022 16:39
%%%-------------------------------------------------------------------
-module(adv_rudy).
-author("gabrielemorello").

%% API
-export([start/1, start/2, stop/0]).
-import(http, [parse_request/1, ok/1]).

safe_init(Port,N) ->
  try init(Port, N)
  catch
    throw:Term -> Term;
    exit:Reason -> {'EXIT',Reason};
    error:Reason:Stk ->
      gen_tcp:close(Port),
      {'EXIT',{Reason,Stk}}
  end.
init(Port, N) ->
  Opt = [list, {active, true}, {reuseaddr, true}, {packet, 2}],
  case gen_tcp:listen(Port, Opt) of
    {ok, Listen} ->
      handlers(Listen, N),
      super();
    {error, Error} ->
      io:format("rudy: init error: ~w~n", [Error]),
      error
  end.

handler(Listen, N) ->
  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      request(Client),
      gen_tcp:close(Client),
      try handler(Listen, N)
      catch
        exit:Reason ->
          gen_tcp:close(Listen),
          {'EXIT',Reason};
        error:Reason:Stk ->
          gen_tcp:close(Listen),
          {'EXIT',{Reason,Stk}}
      end;
    {error, Error} ->
      io:format("rudy: handler error: ~w~n", [Error]),
      error
  end.

request(Client) ->
  case recv_rec(Client) of
    error ->
      io:format("rudy: request error",[]),
      error;
    Str ->
      Request = parse_request(Str),
      Response = reply(Request),
      gen_tcp:send(Client, Response)
  end,
  gen_tcp:close(Client).

reply({{get, _, _}, _, Body}) ->
  factorization:divlist(factorization:divlist(99194853094755497)),
  http:ok(Body).

start(Port, N) ->
  register(rudy, spawn(fun() -> safe_init(Port, N) end)).
start(Port) ->
  start(Port, 1).

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

super() ->
  receive
    stop ->
      ok
  end.

recv_rec(Socket) ->
  receive
    {tcp, Socket, CompleteMsg} ->
      CompleteMsg;
    {tcp_closed, Socket} ->
      io:format("Server closed socket.~n")
  end.
