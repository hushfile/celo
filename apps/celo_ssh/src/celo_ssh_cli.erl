%%%
%%% Copyright (c) 2015 The Celo Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% ----------------------------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc The Celo SSH Dummy CLI.
%%% @end
%%% ----------------------------------------------------------------------------
-module(celo_ssh_cli).

%% API.
-export([start/3]).

-spec start(Username :: string(), Address :: inet:ip_address(), Port :: inet:port_number()) -> pid().
start(Username, Address, Port) ->
    spawn_link(fun () ->
                  %% Register in our process dictionary.
                  put(username, Username),
                  put(address, Address),
                  put(port, Port),

                  %% Write challenge to our peer.
                  {Token, Timestamp} = celo_ssh_challenge:token(Username),

                  NowInSeconds = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(os:timestamp())),
                  TokenTimestampInSeconds = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(Timestamp)),

                  Expires = celo_ssh_config:challenge_timeout() - (NowInSeconds - TokenTimestampInSeconds),

                  io:format("Token expires in ~b seconds~n", [Expires]),
                  io:format("Token: ~s~n", [Token]),

                  %% FIXME(ahf): Maybe add a REPL-shell here once we have
                  %% metadata working.

                  %% Exit our loop.
                  exit(normal)
          end).
