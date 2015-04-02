%%%
%%% Copyright (c) 2015 The Celo Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% ----------------------------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc The Celo Core Utilities API.
%%% @end
%%% ----------------------------------------------------------------------------
-module(celo_core_utilities).

%% API.
-export([expand_tilde/1,
         home_dir/0
        ]).

-spec expand_tilde(Path :: file:filename()) -> file:filename().
expand_tilde(Path) ->
    binary_to_list(iolist_to_binary(re:replace(Path, "~", home_dir()))).

-spec home_dir() -> file:filename().
home_dir() ->
    case os:getenv("HOME") of
        false ->
            [];

        Homedir ->
            Homedir
    end.
