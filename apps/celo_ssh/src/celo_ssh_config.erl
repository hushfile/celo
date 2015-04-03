%%%
%%% Copyright (c) 2015 The Celo Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% ----------------------------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc The Celo SSH Config API.
%%% @end
%%% ----------------------------------------------------------------------------
-module(celo_ssh_config).

%% API.
-export([port/0,
         system_dir/0,
         challenge_timeout/0,
         challenge_passphrase/0
        ]).

%% @doc SSH Port.
-spec port() -> inet:port_number().
port() ->
    celo_core_config:config_value(celo_ssh, port).

%% @doc SSH System Dir.
-spec system_dir() -> file:filename().
system_dir() ->
    filename:join([celo_core_config:home_dir(), celo_core_config:config_value(celo_ssh, system_dir)]).

%% @doc SSH Challenge Timeout.
-spec challenge_timeout() -> pos_integer().
challenge_timeout() ->
    celo_core_config:config_value(celo_ssh, challenge_timeout).

%% @doc SSH Challenge Passphrase.
-spec challenge_passphrase() -> string().
challenge_passphrase() ->
    celo_core_config:config_value(celo_ssh, challenge_passphrase).
