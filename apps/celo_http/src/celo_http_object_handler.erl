%%%
%%% Copyright (c) 2015 The Celo Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% ----------------------------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Object Resource
%%% @end
%%% ----------------------------------------------------------------------------
-module(celo_http_object_handler).

%% API.
-export([init/2,
         malformed_request/2,
         content_types_provided/2,
         to_text/2
        ]).

-record(state, {
    raw_public_key  :: binary(),
    raw_object_name :: binary()
}).

init(Req, _Opts) ->
    RawPublicKey  = cowboy_req:binding(public_key, Req, <<>>),
    RawObjectName = cowboy_req:binding(object_name, Req, <<>>),
    {cowboy_rest, Req, #state {
        raw_public_key  = RawPublicKey,
        raw_object_name = RawObjectName
    }}.

malformed_request(Req, State) ->
    {false, Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"text/plain">>, to_text}
     ], Req, State}.

to_text(Req, State) ->
    {<<"Yo!", $\n>>, Req, State}.
