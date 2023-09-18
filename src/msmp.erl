%% Copyright (c) 2023 Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


-module(msmp).


-export([priv_consult/1]).
-export_type([auth_more_data/0]).
-export_type([auth_switch_request/0]).
-export_type([handshake/0]).
-export_type([handshake_response/0]).
-export_type([pdu/0]).
-export_type([u1/0]).
-export_type([u2/0]).
-export_type([u3/0]).
-export_type([u4/0]).
-export_type([u5/0]).
-export_type([u6/0]).
-export_type([u7/0]).
-export_type([u8/0]).


-type auth_plugin_name() :: mysql_native_password
                          | caching_sha2_password.


-type character_set() :: integer().
-type sequence() :: non_neg_integer().

-type packet() :: handshake()
                | handshake_response().

-type pdu() :: #{packet := packet(), sequence := sequence()}.


-type handshake() :: #{reserved := binary(),
                       action := handshake,
                       character_set := character_set(),
                       capability_flags_1 := msmp_capabilities:lower(),
                       capability_flags_2 := msmp_capabilities:upper(),
                       server_version := binary(),
                       thread_id := integer(),
                       auth_plugin_data_part_1 := binary(),
                       filler := integer(),
                       status_flags := integer(),
                       auth_plugin_data_len := integer(),
                       auth_plugin_data_part_2 := binary(),
                       auth_plugin_name := auth_plugin_name()}.

-type handshake_response() :: #{action := handshake_response,
                                connect_attrs := #{binary() := binary()},
                                client_flags := msmp_capabilities:combined(),
                                character_set := character_set(),
                                auth_response := binary(),
                                client_plugin_name := auth_plugin_name(),
                                max_packet_size := non_neg_integer(),
                                username := binary()}.

-type auth_more_data() :: #{action := auth_more_data,
                            status := fast_auth_success | perform_full_authentication}
                        | #{action := auth_more_data,
                            public_key := binary()}.

-type auth_switch_request() :: #{action := auth_switch_request,
                                 plugin_name := auth_plugin_name(),
                                 plugin_provided_data := binary()}.

-type u1() :: 0..16#ff.
-type u2() :: 0..16#ff_ff.
-type u3() :: 0..16#ff_ff_ff.
-type u4() :: 0..16#ff_ff_ff_ff.
-type u5() :: 0..16#ff_ff_ff_ff_ff.
-type u6() :: 0..16#ff_ff_ff_ff_ff_ff.
-type u7() :: 0..16#ff_ff_ff_ff_ff_ff_ff.
-type u8() :: 0..16#ff_ff_ff_ff_ff_ff_ff_ff.


priv_dir() ->
    code:priv_dir(?MODULE).


priv_consult(Filename) ->
    phrase_file:consult(filename:join(priv_dir(), Filename)).
