%%%
%%% Copyright (c) 2015 The Celo Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% ----------------------------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc The Celo Core Storage Backend Worker
%%% @end
%%% ----------------------------------------------------------------------------
-module(celo_core_storage_backend_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

%% API.
-export([start_link/1]).

%% Gen Server Callbacks.
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, {
    name :: string(),
    callback_module :: atom(),
    callback_options :: proplists:proplist(),
    callback_state :: term()
}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init([Name, Options]) ->
    {CallbackModule, CallbackOptions} = proplists:get_value(backend, Options),
    lager:info("Starting storage backend worker: ~s (~p)", [Name, self()]),
    State = #state {
            name = Name,
            callback_options = CallbackOptions,
            callback_module = CallbackModule,
            callback_state = undefined
        },
    case erlang:function_exported(CallbackModule, init, 2) of
        false ->
            {ok, State};

        true ->
            try
                CallbackState = CallbackModule:init(Name, CallbackOptions),
                {ok, State#state { callback_state = CallbackState }}
            catch
                Class:Reason ->
                    lager:error("Error in storage backend initializer: ~s: ~p/~p", [Name, Class, Reason]),
                    {stop, {error, Reason}}
            end
    end.

handle_call(Request, _From, State) ->
    lager:info("Request: ~p (~p)", [Request, self()]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state { callback_module = CallbackModule, callback_state = CallbackState }) ->
    case erlang:function_exported(CallbackModule, terminate, 2) of
        false ->
            ok;

        true ->
            CallbackModule:terminate(Reason, CallbackState)
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
