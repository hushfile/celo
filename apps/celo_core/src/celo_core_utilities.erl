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
         home_dir/0,
         hex_to_binary/1,
         binary_to_hex/1,
         valid_hex/1
        ]).

-include_lib("celo_core/include/celo_core_test.hrl").

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

%% @doc Convert hex to binary.
-spec hex_to_binary(Data :: binary()) -> binary();
                   (Data :: string()) -> binary().
hex_to_binary([A, B | Rest]) ->
    <<(list_to_integer([A, B], 16)), (hex_to_binary(Rest))/binary>>;

hex_to_binary([A]) ->
    <<(list_to_integer([A], 16))>>;

hex_to_binary([]) ->
    <<>>;

hex_to_binary(Data) when is_binary(Data) ->
    hex_to_binary(binary_to_list(Data)).

%% @doc Convert binary data to hex.
-spec binary_to_hex(Data :: binary()) -> binary();
                   (Data :: string()) -> binary().
binary_to_hex(Data) when is_binary(Data) ->
    list_to_binary(lists:flatten([integer_to_list(X, 16) || <<X:4/integer>> <= Data]));

binary_to_hex(Data) when is_list(Data) ->
    binary_to_hex(list_to_binary(Data)).

%% @doc Check if a given input is a valid hex encoded binary.
-spec valid_hex(Data :: binary()) -> boolean();
               (Data :: string()) -> boolean().
valid_hex(Data) when is_binary(Data) ->
    valid_hex(binary:bin_to_list(Data));

valid_hex(Data) when is_list(Data) ->
    ValidValues = sets:from_list([$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $a, $A, $b, $B, $c, $C, $d, $D, $e, $E, $f, $F]),
    lists:all(fun (Value) -> sets:is_element(Value, ValidValues) end, Data).

-ifdef(TEST).

prop_hex_iso() ->
    ?FORALL(Data, binary(),
        hex_to_binary(binary_to_hex(Data)) =:= Data).

prop_valid_hex() ->
    ?FORALL(Data, binary(),
            valid_hex(binary_to_hex(Data)) =:= true).

-endif.
