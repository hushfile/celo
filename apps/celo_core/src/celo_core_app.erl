%%%
%%% Copyright (c) 2015 The Celo Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% ----------------------------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc The Celo Core Application.
%%% @end
%%% ----------------------------------------------------------------------------
-module(celo_core_app).
-behaviour(application).

%% API.
-export([start/2, stop/1]).

-spec start(normal | {takeover, node()} | {failover, node()}, term()) -> {ok, pid()} | {error, term()}.
start(_Type, _Args) ->
    case celo_core_sup:start_link() of
        {ok, _} = Result ->
            ok = start_storage_backends(),

            Result;

        {error, _} = Error ->
            Error
    end.

-spec stop([]) -> ok.
stop(_State) ->
    ok.

-spec start_storage_backends() -> ok.
start_storage_backends() ->
    StorageBackends = celo_core_config:storage(),
    start_storage_backends(StorageBackends).

-spec start_storage_backends(Backends :: term()) -> ok.
start_storage_backends([]) ->
    ok;

start_storage_backends([{Identifier, Options} | Rest]) ->
    case proplists:get_value(backend, Options) of
        undefined ->
            lager:error("Undefined backend for ~s", [Identifier]);

        {BackendHandler, BackendOptions} ->
            lager:info("Starting storage backend: ~s (~s)", [Identifier, BackendHandler]),

            case erlang:function_exported(BackendHandler, init, 1) of
                true ->
                    try
                        BackendHandler:init(Identifier, BackendOptions)
                    catch
                        Class:Reason ->
                            lager:error("Error in backend initializer for ~s: ~p/~p", [Identifier, Class, Reason])
                    end;

                false ->
                    ok
            end
    end,
    start_storage_backends(Rest).
