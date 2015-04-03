%%%
%%% Copyright (c) 2015 The Celo Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% ----------------------------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc The Celo SSH Callbacks.
%%% @end
%%% ----------------------------------------------------------------------------
-module(celo_ssh).

% API.
-export([auth/2,
         failed_auth/3,
         connect/3,
         disconnect/1
        ]).

-spec auth(Username :: string(), Passphrase :: string()) -> boolean().
auth(Username, Passphrase) ->
    UsernameHex   = list_to_binary(Username),
    PassphraseHex = list_to_binary(Passphrase),
    ChallengePassphrase = list_to_binary(celo_ssh_config:challenge_passphrase()),

    case PassphraseHex of
        ChallengePassphrase ->
            %% Allow access for ChallengePassphrase to read challenge token.
            true;

        _Otherwise ->
            auth_token(UsernameHex, PassphraseHex)
    end.

-spec auth_token(Username :: binary(), Passphrase :: binary()) -> boolean().
auth_token(Username, Passphrase) ->
    %% Ensure that the username is a valid public key and that the passphrase is valid hex.
    ValidHex = celo_core_utilities:valid_hex_public_key(Username) andalso celo_core_utilities:valid_hex(Passphrase),
    case ValidHex of
        false ->
            false;

        true ->
            {Token, _} = celo_ssh_challenge:token(Username),
            auth_token_verify(Username, Passphrase, Token)
    end.

-spec auth_token_verify(Username :: binary(), Passphrase :: binary(), Token :: binary()) -> boolean().
auth_token_verify(Username, Passphrase, Token) ->
    case enacl:sign_open(celo_core_utilities:hex_to_binary(Passphrase), celo_core_utilities:hex_to_binary(Username)) of
        {error, failed_verification} ->
            lager:warning("Challenge: verification failed for ~s", [Username]),
            false;

        {ok, Message} ->
            case Token of
                {Message, _} ->
                    lager:info("Challenge: verification success for ~s", [Username]),
                    true;

                _Otherwise ->
                    lager:warning("Challenge: verification failed for ~s", [Username]),
                    false
            end
    end.

-spec failed_auth(Username :: string(), {Peer :: inet:ip_address(), PeerPort :: inet:port_number()}, Reason :: term()) -> term().
failed_auth(Username, {Peer, PeerPort}, Reason) ->
    lager:warning("SSH: Failed auth from ~s@~s:~b ~s", [Username, inet:ntoa(Peer), PeerPort, Reason]).

-spec connect(Username :: string(), {Peer :: inet:ip_address(), PeerPort :: inet:port_number()}, Method :: string()) -> term().
connect(Username, {Peer, PeerPort}, Method) ->
    lager:notice("SSH: Connect ~s@~s:~b ~s", [Username, inet:ntoa(Peer), PeerPort, Method]).

-spec disconnect(Reason :: term()) -> term().
disconnect(Reason) ->
    lager:notice("SSH: Disconnect: ~s", [Reason]).
