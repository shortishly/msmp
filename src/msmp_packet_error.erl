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


-module(msmp_packet_error).


-export([decode/1]).
-import(scran_bytes, [tag/1]).
-import(scran_combinator, [condition/2]).
-import(scran_combinator, [value/2]).
-import(scran_result, [into_map/1]).
-import(scran_result, [kv/2]).
-import(scran_sequence, [sequence/1]).


decode(ClientFlags) ->
    fun
        (Input) ->
            (into_map(
               sequence(
                 [kv(action, value(error, tag(<<16#ff>>))),
                  kv(error_code, msmp_integer_fixed:decode(2)),
                  condition(
                    maps:get(protocol_41, ClientFlags),
                    sequence(
                     [kv(sql_state_marker, msmp_string_fixed:decode(1)),
                      kv(sql_state, msmp_string_fixed:decode(5))])),
                  kv(error_message, msmp_string_rest_of_packet:decode())])))(Input)
    end.
