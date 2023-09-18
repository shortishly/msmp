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


-module(msmp_binlog_dump).


-export([decode/0]).
-export([encode/0]).


decode() ->
    fun
        (Input) ->
            (scran_result:into_map(
               scran_sequence:sequence(
                 [scran_result:kv(
                    action,
                    scran_combinator:value(
                      binlog_dump,
                      scran_bytes:tag(<<18>>))),

                  scran_result:kv(
                    binlog_pos,
                    msmp_integer_fixed:decode(4)),

                  scran_result:kv(
                    flags,
                    msmp_integer_fixed:decode(2)),

                  scran_result:kv(
                    server_id,
                    msmp_integer_fixed:decode(4)),

                  scran_result:kv(
                    binlog_filename,
                    msmp_string_rest_of_packet:decode())])))(Input)
    end.


encode() ->
    fun
        (Decoded) ->
            (narcs_sequence:sequence(
               [narcs_bytes:tag(<<18>>),
                narcs_combinator:v(binlog_pos, msmp_integer_fixed:encode(4)),
                narcs_combinator:v(flags, msmp_integer_fixed:encode(2)),
                narcs_combinator:v(server_id, msmp_integer_fixed:encode(4)),
                narcs_combinator:v(binlog_filename, narcs_combinator:rest())]))(Decoded)
    end.
