%%%
%%% Copyright (c) 2015 The Celo Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% ----------------------------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc The Celo HTTP Cowboy Middleware.
%%% @end
%%% ----------------------------------------------------------------------------
-module(celo_http_cowboy_middleware).

%% Behaviour.
-behaviour(cowboy_middleware).

%% Cowboy Middleware Behaviour.
-export([execute/2]).

execute(Req, Environment) ->
    Req2 = cowboy_req:set_resp_header(<<"server">>, [celo_core:name(), $/, celo_core:version()], Req),
    {ok, Req2, Environment}.
