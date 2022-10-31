%%%-------------------------------------------------------------------
%%% @author gabrielemorello
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2022 15:20
%%%-------------------------------------------------------------------
-module(node4).
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
  node(Id, Predecessor, Successor, storage:create(), nil, storage:create()).

connect(Id, nil) ->
  {ok, {Id, nil, self()}};
connect(_, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      Sref = monitor(Peer),
      {ok, {Skey, Sref, Peer}}
  after ?Timeout ->
    io:format("Time out: no response~n",[])
  end.

node(Id, Predecessor, Successor, Store, Next, Replica) ->
  receive
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Store, Next, Replica);
    {notify, New} ->
      {Pred, UpdatedStore} = notify(New, Id, Predecessor, Store, Replica),
      node(Id, Pred, Successor, UpdatedStore, Next, Replica);
    {request, Peer} ->
      request(Peer, Predecessor, Successor),
      node(Id, Predecessor, Successor, Store, Next, Replica);
    {status, Pred, Nx} ->
      {Succ, Nxt} = stabilize(Pred, Id, Successor, Nx),
      node(Id, Predecessor, Succ, Store, Nxt, Replica);
    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor, Store, Next, Replica);

    {add, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Added, Next, Replica);
    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Store, Next, Replica);
    {handover, Elements, ReplicaAdd} ->
      Merged = storage:merge(Store, Elements),
%  TODO    Merged1 = storage:merge(Merged, ReplicaAdd),
      {_,_,Spid} = Successor,
      Spid ! {replica, Merged},
      node(Id, Predecessor, Successor, Merged, Next, Replica);
    {'DOWN', Ref, process, _, _} ->
      {Pred, Succ, Nxt, Store1} = down(Ref, Predecessor, Successor, Next, Store, Replica),
      node(Id, Pred, Succ, Store1, Nxt, storage:create());

    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor, Store, Next, Replica);
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Store, Next, Replica);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor, Store, Next, Replica);

    {replicate, Client, Qref, Key, Value} ->
      NewReplica = storage:add(Key, Value, Replica),
      Client ! {Qref, ok},
      node(Id, Predecessor, Successor, Store, Next, NewReplica);
    {replica, NewReplica} ->
      node(Id, Predecessor, Successor, Store, Next, NewReplica);

    status ->
      io:format("Node: ~p, Predecessor: ~p, Successor: ~p, Next: ~p~n", [Id, Predecessor, Successor, Next]),
      node(Id, Predecessor, Successor, Store, Next, Replica);
    stop ->
      ok
  end.

add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Spid ! {replicate, Client, Qref, Key, Value},
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

handover(Id, Store, Nkey, Npid, Replica) ->
  {Rest, Keep} = storage:split(Id, Nkey, Store),
  {ReplicaRest, ReplicaKeep} = storage:split(Id, Nkey, Replica), % TODO ???
  Npid ! {handover, Rest, ReplicaRest},
  {Keep, ReplicaKeep}.

create_probe(Id, {_, _, Spid}) ->
  Spid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.

remove_probe(Time, Nodes) ->
  T = erlang:system_time(micro_seconds) - Time,
  io:format("Probe time: ~w~n Nodes: ~w", [T, Nodes]).

forward_probe(Ref, Time, Nodes, Id, {_, _, Spid}) ->
  Spid ! {probe, Ref, [Id | Nodes], Time}.

stabilize({_, _, Spid}) ->
  Spid ! {request, self()}.
stabilize(Pred, Id, Successor, Next) ->
  {Skey, Sref, Spid} = Successor,
  case Pred of
    nil ->
      Spid ! {notify, {Id, self()}},
      {Successor, Next};
    {Id, _} ->
      {Successor, Next};
    {Skey, _} ->
      Spid ! {notify, {Id, self()}},
      {Successor, Next};
    {Xkey, Xpid} ->
      case key:between(Xkey, Id, Skey) of
        true ->
          Xpid ! {request, self()},
          Xref = monitor(Xpid),
          drop(Sref),
          {{Xkey, Xref, Xpid}, {Skey, Spid}};
        false ->
          Spid ! {notify, {Id, self()}},
          {Successor, Next}
      end
  end.

schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).   % sends a stabilize message to self() repeatedly after ?Stabilize milliseconds

request(Peer, Predecessor, {Skey,_,Spid}) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil, {Skey, Spid}};
    {Pkey, _, Ppid} ->
      Peer ! {status, {Pkey, Ppid}, {Skey,Spid}}
  end.

notify({Nkey, Npid}, Id, Predecessor, Store, Replica) ->
  case Predecessor of
    nil ->
      Keep = handover(Id, Store, Nkey, Npid, Replica),
      Nref = monitor(Npid),
      {{Nkey, Nref, Npid}, Keep};
    {Pkey, Pref, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          Keep = handover(Id, Store, Nkey, Npid, Replica),
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

down(Ref, {_, Ref, _}, Successor, Next, Store, Replica) ->
  NewStore = storage:merge(Store, Replica),
  {nil, Successor, Next, NewStore};
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}, Store, Replica) ->
  Nref = monitor(Npid),
  Npid ! {replica, Store},
  {Predecessor, {Nkey, Nref, Npid}, nil, Store}.