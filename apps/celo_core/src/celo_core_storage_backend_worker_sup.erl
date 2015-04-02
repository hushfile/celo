%%%
%%% Copyright (c) 2015 The Celo Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% ----------------------------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc The Celo Core Storage Backend Worker Supervisor
%%% @end
%%% ----------------------------------------------------------------------------
-module(celo_core_storage_backend_worker_sup).

-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% Supervisor Callbacks.
-export([init/1]).

%% From supervisor.
-type start_link_err() :: {already_started, pid()} | shutdown | term().
-type start_link_ret() :: {ok, pid()} | ignore | {error, start_link_err()}.

-spec start_link() -> start_link_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {{one_for_one, non_neg_integer(), non_neg_integer()}, []}}.
init([]) ->
    Procs = lists:map(fun ({Name, Options}) ->
                              PoolArgs = [
                                    {name, {local, Name}},
                                    {worker_module, celo_core_storage_backend_worker},
                                    {size, 10},
                                    {max_overflow, 20}
                                 ],
                              WorkerArgs = [Name, Options],
                              poolboy:child_spec(Name, PoolArgs, WorkerArgs)
                      end, celo_core_config:storage()),
    {ok, {{one_for_one, 10, 10}, Procs}}.
