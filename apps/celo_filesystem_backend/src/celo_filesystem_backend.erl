%%%
%%% Copyright (c) 2015 The Celo Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% ----------------------------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc The Celo Filesystem Backend.
%%% @end
%%% ----------------------------------------------------------------------------
-module(celo_filesystem_backend).

-export([init/2,
         object_exists/3,
         object_size/3,
         object_stream/3
        ]).

-record(state, {
    name :: atom(),
    path :: file:filename(),
    options :: proplists:proplist()
}).

init(Name, Options) ->
    Path = celo_core_utilities:expand_tilde(proplists:get_value(path, Options)),

    lager:info("Initializing ~p in ~s", [Name, Path]),

    case filelib:ensure_dir(Path) of
        ok ->
            {ok, #state {
                    name = Name,
                    path = Path,
                    options = Options
                   }};

        {error, _} = Error ->
            Error
    end.


object_exists(PublicKey, ObjectId, State) ->
    Path = path(State, PublicKey, ObjectId),
    {filelib:is_file(Path), State}.

object_size(PublicKey, ObjectId, State) ->
    Path = path(State, PublicKey, ObjectId),
    {filelib:file_size(Path), State}.

object_stream(PublicKey, ObjectId, State) ->
    Path = path(State, PublicKey, ObjectId),
    Fun = fun (Socket, Transport) ->
              Transport:sendfile(Socket, Path)
          end,
    {Fun, State}.

path(State, PublicKey, ObjectId) ->
    {PKHead, PKTail} = split(celo_core_utilities:binary_to_hex(PublicKey)),
    {IDHead, IDTail} = split(celo_core_utilities:binary_to_hex(ObjectId)),
    filename:join([State#state.path, PKHead, PKTail, IDHead, IDTail]).

split(Data) ->
    Size = byte_size(Data),
    Head = binary_to_list(binary:part(Data, 0, 4)),
    Tail = binary_to_list(binary:part(Data, 4, Size - 4)),
    {Head, Tail}.
