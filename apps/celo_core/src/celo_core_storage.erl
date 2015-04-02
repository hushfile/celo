%%%
%%% Copyright (c) 2015 The Celo Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% ----------------------------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Celo Core Object Store.
%%% @end
%%% ----------------------------------------------------------------------------
-module(celo_core_storage).

%% API.
-export([object_exists/2,
         object_size/2,
         object_stream/4
        ]).

%% @doc Check if a given object exists.
-spec object_exists(PublicKey :: binary(), ObjectId :: binary()) -> boolean().
object_exists(PublicKey, ObjectId) ->
    dispatch({object_exists, PublicKey, ObjectId}).

%% @doc Get object size in bytes.
-spec object_size(PublicKey :: binary(), ObjectId :: binary()) -> {ok, pos_integer()} | {error, any()}.
object_size(PublicKey, ObjectId) ->
    dispatch({object_size, PublicKey, ObjectId}).

%% @doc Fetch object as a stream.
-spec object_stream(PublicKey :: binary(), ObjectId :: binary(), Socket :: inet:socket(), Transport :: atom()) -> ok | {error, any()}.
object_stream(PublicKey, ObjectId, Socket, Transport) ->
    dispatch({object_stream, PublicKey, ObjectId, Socket, Transport}).

%% @private
dispatch(Event) ->
    Backends = lists:map(fun ({Name, _}) -> Name end, celo_core_config:storage()),
    dispatch(Event, Backends).

%% @private
dispatch(Event, []) ->
    lager:error("No backend handler for: ~p", [Event]),
    error;

dispatch(Event, [Backend | Rest]) ->
    lager:info("Dispatching event (~p) to ~p", [Event, Backend]),
    case poolboy:transaction(Backend, fun (Worker) -> gen_server:call(Worker, Event) end) of
        ok ->
            ok;

        _Otherwise ->
            dispatch(Event, Rest)
    end.
