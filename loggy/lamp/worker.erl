%%%-------------------------------------------------------------------
%%% @author gabrielemorello
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Sep 2022 13:33
%%%-------------------------------------------------------------------
-module(worker).
-author("gabrielemorello").

%% API
-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
  spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
  Worker ! stop.
init(Name, Log, Seed, Sleep, Jitter) ->
  random:seed(Seed, Seed, Seed),
  receive
    {peers, Peers} ->
      loop(Name, Log, Peers, Sleep, Jitter, time:zero());
    stop ->
      ok
  end.

peers(Wrk, Peers) ->
  Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, OldTime)->
  Wait = random:uniform(Sleep),
  receive
    {msg, Time, Msg} ->
      NewVect = time:merge(OldTime, Time),
      NewTime = time:inc(Name, NewVect),
      Log ! {log, Name, NewTime, {received, Msg}},
      loop(Name, Log, Peers, Sleep, Jitter, NewTime);
    stop ->
      ok;
    Error ->
      Log ! {log, Name, time, {error, Error}}
  after Wait ->
    Selected = select(Peers),
    NewTime = time:inc(Name, OldTime),
    Message = {hello, random:uniform(1000)},
    Selected ! {msg, NewTime, Message},
    jitter(Jitter),
    Log ! {log, Name, NewTime, {sending, Message}},
    loop(Name, Log, Peers, Sleep, Jitter, NewTime)
  end.

select(Peers) ->
  lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).