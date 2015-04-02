%%%
%%% Copyright (c) 2015 The Celo Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% ----------------------------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc The Celo HTTP Application.
%%% @end
%%% ----------------------------------------------------------------------------
-module(celo_http_app).
-behaviour(application).

%% API.
-export([start/2, stop/1]).

-spec start(normal | {takeover, node()} | {failover, node()}, term()) -> {ok, pid()} | {error, term()}.
start(_Type, _Args) ->
    case celo_http_sup:start_link() of
        {ok, _} = Result ->
            Dispatch = cowboy_router:compile([
                {'_', []}
            ]),

            {ok, _} = cowboy:start_https(celo_https, 1000, [
                {cacertfile, celo_http_config:tls_ca_file()},
                {certfile, celo_http_config:tls_cert_file()},
                {dhfile, celo_http_config:tls_dh_file()},
                {keyfile, celo_http_config:tls_key_file()},
                {port, celo_http_config:tls_port()}
            ], [
                {env, [
                    {dispatch, Dispatch}
                ]},

                {middlewares, [celo_http_cowboy_middleware, cowboy_router, cowboy_handler]}
            ]),

            Result;

        {error, _} = Error ->
            Error
    end.

-spec stop([]) -> ok.
stop(_State) ->
    ok.
