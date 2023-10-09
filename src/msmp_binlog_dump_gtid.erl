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


-module(msmp_binlog_dump_gtid).


-export([decode/0]).
-export([encode/0]).

decode() ->
    fun
        (Encoded) ->
            (scran_result:into_map(
               scran_sequence:sequence(
                 [scran_result:kv(
                    action,
                    scran_combinator:value(
                      binlog_dump_gtid,
                      scran_bytes:tag(command()))),

                  scran_result:kv(
                    flags,
                    msmp_integer_fixed:decode(2)),

                  scran_result:kv(
                    server_id,
                    msmp_integer_fixed:decode(4)),

                  scran_result:kv(
                    name,
                    scran_bytes:take(
                      msmp_integer_fixed:decode(4))),

                  scran_result:kv(
                    position,
                    msmp_integer_fixed:decode(8)),

                  scran_result:kv(
                    gtids,
                    scran_combinator:map_parser(
                      scran_bytes:take(
                        msmp_integer_fixed:decode(4)),
                      msmp_gtid_set:decode()))])))(Encoded)
    end.


encode() ->
    fun
        (Decoded) ->
            (narcs_sequence:sequence(
               [narcs_bytes:tag(command()),
                narcs_combinator:v(flags, msmp_integer_fixed:encode(2)),
                narcs_combinator:v(server_id, msmp_integer_fixed:encode(4)),
                narcs_combinator:v(
                  name,
                  narcs_bytes:length_encoded(msmp_integer_fixed:encode(4))),
                narcs_combinator:v(position, msmp_integer_fixed:encode(8)),
                narcs_combinator:v(
                  gtids,
                  narcs_combinator:map_result(
                    msmp_gtid_set:encode(),
                    narcs_bytes:length_encoded(
                      msmp_integer_fixed:encode(4))))]))(Decoded)
    end.


command() ->
    <<30>>.
