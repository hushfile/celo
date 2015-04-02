%%%
%%% Copyright (c) 2015 The Celo Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% ----------------------------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc The Celo HTTP Config API.
%%% @end
%%% ----------------------------------------------------------------------------
-module(celo_http_config).

%% API.
-export([tls_port/0,
         tls_ca_file/0,
         tls_cert_file/0,
         tls_dh_file/0,
         tls_key_file/0
        ]).

%% @doc TLS Port.
-spec tls_port() -> inet:port_number().
tls_port() ->
    celo_core_config:config_value(celo_http, tls, port).

%% @doc TLS CA File.
-spec tls_ca_file() -> file:filename().
tls_ca_file() ->
    CAFile = celo_core_config:config_value(celo_http, tls, ca_file),
    filename:join([celo_core_config:home_dir(), CAFile]).

%% @doc TLS Cert File.
-spec tls_cert_file() -> file:filename().
tls_cert_file() ->
    CertFile = celo_core_config:config_value(celo_http, tls, cert_file),
    filename:join([celo_core_config:home_dir(), CertFile]).

%% @doc TLS DH File.
-spec tls_dh_file() -> file:filename().
tls_dh_file() ->
    DHFile = celo_core_config:config_value(celo_http, tls, dh_file),
    filename:join([celo_core_config:home_dir(), DHFile]).

%% @doc TLS Private Key File.
-spec tls_key_file() -> file:filename().
tls_key_file() ->
    KeyFile = celo_core_config:config_value(celo_http, tls, key_file),
    filename:join([celo_core_config:home_dir(), KeyFile]).
