%%%
%%% Copyright (c) 2015 The Celo Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% ----------------------------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc The Celo SSH Application.
%%% @end
%%% ----------------------------------------------------------------------------
-module(celo_ssh_app).
-behaviour(application).

%% API.
-export([start/2, stop/1]).

-spec start(normal | {takeover, node()} | {failover, node()}, term()) -> {ok, pid()} | {error, term()}.
start(_Type, _Args) ->
    celo_ssh_sup:start_link().

-spec stop([]) -> ok.
stop(_State) ->
    ok.
