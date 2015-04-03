%%%
%%% Copyright (c) 2015 The Celo Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% ----------------------------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc The Celo SSH Supervisor.
%%% @end
%%% ----------------------------------------------------------------------------
-module(celo_ssh_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% Supervisor callbacks.
-export([init/1]).

%% From supervisor.
-type start_link_err() :: {already_started, pid()} | shutdown | term().
-type start_link_ret() :: {ok, pid()} | ignore | {error, start_link_err()}.

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-spec start_link() -> start_link_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SFTPRoot = filename:join([celo_ssh_config:system_dir(), "root"]),
    file:make_dir(SFTPRoot),
    SSHOptions = [celo_ssh_config:port(), [
                        % Path to our host key.
                        {system_dir, celo_ssh_config:system_dir()},

                        % Disable shell access.
                        {shell, fun (Username, {Address, Port}) ->
                                        celo_ssh_cli:start(Username, Address, Port)
                                end},

                        % SFTP.
                        {subsystems, [ssh_sftpd:subsystem_spec([
                                        {root, SFTPRoot}
                                     ])]},

                        % Password authentication function.
                        {pwdfun, fun celo_ssh:auth/2},

                        % Failed authentication function.
                        {failfun, fun celo_ssh:failed_auth/3},

                        % Connection function.
                        {connectfun, fun celo_ssh:connect/3},

                        % Disconnect function.
                        {disconnectfun, fun celo_ssh:disconnect/1}
                    ]
                 ],
    Procs = [
             {ssh, {ssh, daemon, SSHOptions}, permanent, 5000, worker, [ssh]},
             ?CHILD(celo_ssh_challenge, worker)
            ],
    {ok, {{one_for_one, 10, 10}, Procs}}.
