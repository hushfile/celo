%%%
%%% Copyright (c) 2015 The Celo Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% ----------------------------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc The Celo Core Config API.
%%% @end
%%% ----------------------------------------------------------------------------
-module(celo_core_config).

%% API.
-export([home_dir/0]).

%% Config API.
-export([config_value/2,
         config_value/3
        ]).

-type key()   :: atom().
-type value() :: any().

%% @doc Fetch Celo's home directory.
-spec home_dir() -> file:filename().
home_dir() ->
    celo_core_utilities:expand_tilde(config_value(celo_core, home_dir)).

%% @doc Fetch Key from a given application.
-spec config_value(App :: atom(), Key :: key()) -> value().
config_value(App, Key) ->
    case application:get_env(App, Key) of
        undefined ->
            exit({missing_config, App, Key});

        {ok, Value} ->
            Value
    end.

%% @doc Fetch a given Key/SubKey from a given application.
-spec config_value(App :: atom(), Key :: key(), SubKey :: key()) -> value().
config_value(App, Key, SubKey) ->
    case application:get_env(App, Key) of
        undefined ->
            exit({missing_config, App, Key, SubKey});

        {ok, Proplist} ->
            case proplists:get_value(SubKey, Proplist) of
                undefined ->
                    exit({missing_config, App, Key, SubKey});

                Value ->
                    Value
            end
    end.
