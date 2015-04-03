%%%
%%% Copyright (c) 2015 The Celo Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% ----------------------------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc The Celo SSH Challenge handler.
%%% @end
%%% ----------------------------------------------------------------------------
-module(celo_ssh_challenge).

-behaviour(gen_server).

%% API.
-export([start_link/0,
         token/1,
         delete/1
        ]).

%% Gen Server Callbacks.
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, {
          table :: ets:tid()
         }).

-record(challenge, {
          username  :: string(),
          challenge :: string(),
          timestamp :: erlang:timestamp()
         }).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec token(Username :: string()) -> string().
token(Username) ->
    gen_server:call(?MODULE, {token, Username}).

-spec delete(Username :: string()) -> ok.
delete(Username) ->
    gen_server:call(?MODULE, {delete, Username}).

init([]) ->
    {ok, #state {
        table = ets:new(?MODULE, [protected, {keypos, #challenge.username}])
    }}.

handle_call({token, Username}, _From, #state { table = Table } = State) ->
    Token = case ets:lookup(Table, Username) of
               [] ->
                    NewToken = celo_core_utilities:binary_to_hex(crypto:hash(sha256, enacl:randombytes(crypto:rand_uniform(100, 200)))),
                    Timestamp = os:timestamp(),
                    lager:info("Challenge: Adding ~s -> ~s", [Username, NewToken]),
                    ets:insert(Table, #challenge { username = Username, challenge = NewToken, timestamp = Timestamp }),
                    timer:apply_after(timer:seconds(celo_ssh_config:challenge_timeout()), ?MODULE, delete, [Username]),
                    {NewToken, Timestamp};

               [#challenge { username = Username, challenge = NewToken, timestamp = Timestamp }] ->
                    {NewToken, Timestamp}
            end,
    {reply, Token, State};

handle_call({delete, Username}, _From, #state { table = Table } = State) ->
    lager:info("Challenge: Deleting: ~s", [Username]),
    ets:delete(Table, Username),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
