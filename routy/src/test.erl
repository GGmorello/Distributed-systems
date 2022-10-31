%%%-------------------------------------------------------------------
%%% @author gabrielemorello
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Sep 2022 00:24
%%%-------------------------------------------------------------------
-module(test).
-author("gabrielemorello").

%% API
-export([create_net/0, italy/0, sweden/0, stop/0]).

italy() ->
  routy:start(r1, roma),
  routy:start(r2, torino),
  routy:start(r3, milano),
  roma ! {add, torino, {torino, 'italy@0.0.0.0'}},
  torino ! {add, roma, {roma,'italy@0.0.0.0'}},
  milano ! {add, roma, {roma,'italy@0.0.0.0'}},
  roma ! {add, milano, {milano, 'italy@0.0.0.0'}}.

sweden() ->
  routy:start(r3, stockholm),
  routy:start(r4, malmo),
  stockholm ! {add, malmo, {malmo, 'sweden@0.0.0.0'}},
  malmo ! {add, stockholm, {stockholm,'sweden@0.0.0.0'}}.

connect_italy_sweden() ->
  {stockholm, 'sweden@0.0.0.0'} ! {add, roma, {roma, 'italy@0.0.0.0'}},
  {roma, 'italy@0.0.0.0'} ! {add, stockholm, {stockholm, 'sweden@0.0.0.0'}}.

create_net() ->
  connect_italy_sweden(),
  timer:sleep(2000),
  {stockholm, 'sweden@0.0.0.0'} ! broadcast,
  {malmo, 'sweden@0.0.0.0'} ! broadcast,
  {roma, 'italy@0.0.0.0'} ! broadcast,
  {torino, 'italy@0.0.0.0'} ! broadcast,
  {milano, 'italy@0.0.0.0'} ! broadcast,
  timer:sleep(2000),
  {stockholm, 'sweden@0.0.0.0'} ! update,
  {malmo, 'sweden@0.0.0.0'} ! uhite,
  {roma, 'italy@0.0.0.0'} ! update,
  {torino, 'italy@0.0.0.0'} ! update,
  {milano, 'italy@0.0.0.0'} ! update.

stop() ->
  {torino, 'italy@0.0.0.0'} ! stop,
  {stockholm, 'sweden@0.0.0.0'} ! stop,
  {malmo, 'sweden@0.0.0.0'} ! stop,
  {roma, 'italy@0.0.0.0'} ! stop,
  {milano, 'italy@0.0.0.0'} ! stop.

