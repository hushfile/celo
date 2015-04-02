%%%
%%% Copyright (c) 2015 The Celo Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
-ifdef(TEST).

-include_lib("triq/include/triq.hrl").
-include_lib("eunit/include/eunit.hrl").

-spec property_test() -> any().
property_test() ->
    ?assert(triq:check(?MODULE)).

-endif.
