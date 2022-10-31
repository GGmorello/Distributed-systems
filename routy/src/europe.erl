%%%-------------------------------------------------------------------
%%% @author gabrielemorello, perttujääskeläinen, jonathanarns, fabianzeiher
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Sep 2022 14:04
%%%-------------------------------------------------------------------
-module(europe).
-author(["gabrielemorello", "perttujääskeläinen", "jonathanarns", "fabianzeiher"]).

%% API
-export([italy/0, create_net/0, message/1, stop_italy/0, broadcasts/0, connect_ita_aus/0, connect_ita_ger/0, connect_swe_ger/0]).

-define(SweNode, 'sweden5-p9.eduroam.kth.se').
-define(ItaNode, 'italia@n165-p24.eduroam.kth.se').
-define(GerNode, 'germany@130.229.156.30').
-define(AusNode, 'austria@n166-p255.eduroam.kth.se').

italy() ->
  routy:start(roma),
  routy:start(torino),
  routy:start(napoli),
  {roma, ?ItaNode} ! {add, napoli, {napoli, ?ItaNode}},
  {roma, ?ItaNode} ! {add, torino, {torino, ?ItaNode}},
  {torino, ?ItaNode} ! {add, roma, {roma, ?ItaNode}},
  {napoli, ?ItaNode} ! {add, roma, {roma, ?ItaNode}}.

connect_ita_ger() ->
  {roma, ?ItaNode} ! {add, berlin, {berlin, ?GerNode}},
  {berlin, ?GerNode} ! {add, roma, {roma, ?ItaNode}}.


connect_ita_swe() ->
  {roma, ?ItaNode} ! {add, stockholm, {stockholm, ?SweNode}},
  {stockholm, ?SweNode} ! {add, roma, {roma, ?ItaNode}}.


connect_ita_aus() ->
  {roma, ?ItaNode} ! {add, wien, {wien, ?AusNode}},
  {wien, ?AusNode} ! {add, roma, {roma, ?ItaNode}}.

connect_swe_ger() ->
  {stockholm, ?SweNode} ! {add, berlin, {berlin, ?GerNode}},
  {berlin, ?GerNode} ! {add, stockholm, {stockholm, ?SweNode}}.


create_net() ->
  connect_ita_aus(),
  connect_ita_ger(),
  connect_ita_swe().

broadcasts() ->
  {roma, ?ItaNode} ! broadcast,
  {torino, ?ItaNode} ! broadcast,
  {napoli, ?ItaNode} ! broadcast,
  timer:sleep(1000),
  {roma, ?ItaNode} ! update,
  {torino, ?ItaNode} ! update,
  {napoli, ?ItaNode} ! update.

message(Str) ->
  {roma, ?ItaNode} ! {send, stockholm, Str}.

stop_italy() ->
  torino ! stop,
  napoli ! stop,
  roma ! stop.