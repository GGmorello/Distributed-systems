%%%-------------------------------------------------------------------
%%% @author gabrielemorello
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2022 15:20
%%%-------------------------------------------------------------------
-module(node3).
-author("gabrielemorello").

%% API
-compile(export_all).

-define(Timeout, 1000).
-define(Stabilize, 100).


start(Id) ->
  start(Id, nil).

start(Id, Peer) ->
  timer:start(),
  spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
  Predecessor = nil,
  {ok, Successor} = connect(Id, Peer),
  schedule_stabilize(),
  node(Id, Predecessor, Successor, storage:create(), nil).

connect(Id, nil) ->
  {ok, {Id, nil, self()}};

connect(Id, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      Sref = monitor(Peer),
      {ok, {Skey, Sref, Peer}}
  after ?Timeout ->
    io:format("Time out: no response~n",[])
  end.

schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).   % sends a stabilize message to self() repeatedly after ?Stabilize milliseconds

node(Id, Predecessor, Successor, Store, Next) ->
  receive
    {key, Qref, Peer} -> % a peer needs to know our key
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Store, Next);
    {notify, New} -> % a new node informs us of its existence
      {Pred, UpdatedStore} = notify(New, Id, Predecessor, Store),
      node(Id, Pred, Successor, UpdatedStore, Next);
    {request, Peer} -> % a predecessor needs to know our predecessor
      request(Peer, Predecessor, Successor),
      node(Id, Predecessor, Successor, Store, Next);
    {status, Pred, Nx} -> % our successor informs us about its predecessor
      {Succ, Nxt} = stabilize(Pred, Id, Successor, Nx),
      node(Id, Predecessor, Succ, Store, Nxt);
    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor, Store, Next);

    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor, Store, Next);
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Store, Next);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor, Store, Next);

    {add, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Added, Next);

    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Store, Next);

    {handover, Elements} ->
      Merged = storage:merge(Store, Elements),
      node(Id, Predecessor, Successor, Merged, Next);

    {'DOWN', Ref, process, _, _} ->
      {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
      node(Id, Pred, Succ, Store, Nxt);
    state ->
      io:format("Id: ~w~n", [Id]),
      io:format("Predecessor: ~p, Successor: ~p, Next: ~p~n", [Predecessor, Successor, Next]),
      node(Id, Predecessor, Successor, Store, Next);
    stop ->
      ok
  end.

handover(Id, Store, Nkey, Npid) ->
  {Rest, Keep} = storage:split(Id, Nkey, Store),
  Npid ! {handover, Rest},
  Keep.

add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Client ! {Qref, ok},
      storage:add(Key, Value, Store);
    false ->
      Spid ! {add, Key, Value, Qref, Client},
      Store
  end.

lookup(Key, Qref, Client, Id, {Pkey, _, _}, Successor, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Result = storage:lookup(Key, Store),
      Client ! {Qref, Result};
    false ->
      {_,_, Spid} = Successor,
      Spid ! {lookup, Key, Qref, Client}
  end.

create_probe(Id, {_, _, Spid}) ->
  Spid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.

remove_probe(Time, Nodes) ->
  T = erlang:system_time(micro_seconds) - Time,
  io:format("Probe time: ~w micro seconds ~n Nodes: ~w", [T, Nodes]).

forward_probe(Ref, Time, Nodes, Id, {_, _, Spid}) ->
  Spid ! {probe, Ref, [Id | Nodes], Time}.

stabilize(Pred, Id, Successor, Next) ->
  {Skey, Sref, Spid} = Successor,
  case Pred of
    nil -> % inform about our existence
      Spid ! {notify, {Id, self()}},
      {Successor, Next};
    {Id, _} -> % nothing happens
      {Successor, Next};
    {Skey, _} -> % inform about our existence
      Spid ! {notify, {Id, self()}},
      {Successor, Next};
    {Xkey, Xpid} ->
      case key:between(Xkey, Id, Skey) of
        true -> % adopt predecessor
          Xpid ! {request, self()},
          Xref = monitor(Xpid),
          drop(Sref),
          {{Xkey, Xref, Xpid}, {Skey, Spid}};
        false ->
          Spid ! {notify, {Id, self()}},
          {Successor, Next}
      end
  end.

stabilize({_, _, Spid}) ->
  Spid ! {request, self()}.

request(Peer, Predecessor, {Skey,_,Spid}) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil, {Skey, Spid}};
    {Pkey, _, Ppid} ->
      Peer ! {status, {Pkey, Ppid}, {Skey,Spid}}
  end.


notify({Nkey, Npid}, Id, Predecessor, Store) ->

  case Predecessor of
    nil ->
      Keep = handover(Id, Store, Nkey, Npid),
      Nref = monitor(Npid),
      {{Nkey, Nref, Npid}, Keep};
    {Pkey, Pref, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          Keep = handover(Id, Store, Nkey, Npid),
          Nref = monitor(Npid),
          drop(Pref),
          {{Nkey, Nref, Npid}, Keep};
        false ->
          {Predecessor, Store}
      end
  end.

monitor(Pid) ->
  erlang:monitor(process, Pid).

drop(nil) ->
  ok;
drop(Pid) ->
  erlang:demonitor(Pid, [flush]).

down(Ref, {_, Ref, _}, Successor, Next) ->
  {nil, Successor, Next};

down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}) ->
  io:format("Successor of ~w died~n", [Ref]),
  Nref = monitor(Npid),
  Npid ! stabilize,
  {Predecessor, {Nkey, Nref, Npid}, nil}.