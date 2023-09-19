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


-module(msmp_ssl_request).


-export([decode/0]).
-export([encode/0]).


decode() ->
    fun
        (Encoded) ->
            (scran_result:into_map(
               scran_sequence:sequence(
                 [scran_result:kv(
                    client_flags,
                    msmp_capabilities:decode(combined)),

                  scran_result:kv(
                    max_packet_size,
                    msmp_integer_fixed:decode(4)),

                  scran_result:kv(
                    character_set,
                    msmp_integer_fixed:decode(1)),

                  scran_result:kv(
                    filler,
                    scran_bytes:tag(<<0:23/unit:8>>)),

                  scran_result:kv(
                    action,
                    scran_combinator:value(
                      ssl_request,
                      scran_combinator:eof()))])))(Encoded)
    end.

encode() ->
    fun
        (#{action := ssl_request, extended_capabilities := _} = Decoded) ->
            (narcs_sequence:sequence(
               [narcs_combinator:v(client_flags, msmp_capabilities:encode(combined)),
                narcs_combinator:v(max_packet_size, msmp_integer_fixed:encode(4)),
                narcs_combinator:v(character_set, msmp_integer_fixed:encode(1)),
                narcs_bytes:tag(<<0:19/unit:8>>),
                narcs_combinator:v(
                  extended_capabilities,
                  msmp_integer_fixed:encode(4))]))(Decoded);

        (#{action := ssl_request} = Decoded) ->
            (narcs_sequence:sequence(
               [narcs_combinator:v(client_flags, msmp_capabilities:encode(combined)),
                narcs_combinator:v(max_packet_size, msmp_integer_fixed:encode(4)),
                narcs_combinator:v(character_set, msmp_integer_fixed:encode(1)),
                narcs_bytes:tag(<<0:23/unit:8>>)]))(Decoded)
    end.
