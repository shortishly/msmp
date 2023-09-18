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


-module(msmp_register_replica).


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
                      register_replica, scran_bytes:tag(<<21>>))),
                  scran_result:kv(server_id, msmp_integer_fixed:decode(4)),
                  scran_result:kv(host, msmp_string_length_encoded:decode()),
                  scran_result:kv(user, msmp_string_length_encoded:decode()),
                  scran_result:kv(password, msmp_string_length_encoded:decode()),
                  scran_result:kv(port, msmp_integer_fixed:decode(2)),
                  scran_result:kv(recovery_rank, scran_bytes:take(4)),
                  scran_result:kv(master_id, msmp_integer_fixed:decode(4))])))(Encoded)
    end.


encode() ->
    fun
        (Decoded) ->
            (narcs_sequence:sequence(
               [narcs_bytes:tag(<<21>>),
                narcs_combinator:v(server_id, msmp_integer_fixed:encode(4)),
                narcs_combinator:v(host, msmp_string_length_encoded:encode()),
                narcs_combinator:v(user, msmp_string_length_encoded:encode()),
                narcs_combinator:v(password, msmp_string_length_encoded:encode()),
                narcs_combinator:v(port, msmp_integer_fixed:encode(2)),
                narcs_combinator:v(recovery_rank, narcs_combinator:rest()),
                narcs_combinator:v(master_id, msmp_integer_fixed:encode(4))]))(Decoded)
    end.
