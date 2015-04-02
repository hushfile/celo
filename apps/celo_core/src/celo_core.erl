%%%
%%% Copyright (c) 2015 The Celo Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% ----------------------------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc The Celo Core API.
%%% @end
%%% ----------------------------------------------------------------------------
-module(celo_core).

%% API.
-export([name/0,
         version/0,
         platform/0
        ]).

-spec name() -> string().
name() ->
    "Celo".

-spec version() -> string().
version() ->
    {ok, Version} = application:get_key(celo_core, vsn),
    Version.

-spec platform() -> string().
platform() ->
    ErlangVersion = erlang:system_info(otp_release),
    {Family, Name} = os:type(),
    lists:flatten(io_lib:format("~s-~s on ~s/~s (OTP ~s)", [name(), version(), Family, Name, ErlangVersion])).
