%%%
%%% Copyright (c) 2015 The Celo Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% ----------------------------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc The Celo Core Types.
%%% @end
%%% ----------------------------------------------------------------------------
-module(celo_core_types).

%% Types.
-export_type([storage_backend/0,
              storage_spec/0,
              storage_option_spec/0,
              storage_backend_options/0
             ]).

-type storage_backend() :: celo_filesystem_backend.

-type storage_spec() :: {StorageIdentifier :: string(), StorageOptions :: [storage_option_spec()]}.

-type storage_option_spec() :: {backend, storage_backend(), storage_backend_options()}.

-type storage_backend_options() :: {path, string()}.
