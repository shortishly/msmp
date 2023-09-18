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


-module(msmp_handshake).


-export([decode/0]).
-import(scran_branch, [alt/1]).
-import(scran_bytes, [null_terminated/0]).
-import(scran_bytes, [tag/1]).
-import(scran_bytes, [take/1]).
-import(scran_combinator, [value/2]).
-import(scran_result, [into_atom/1]).
-import(scran_result, [into_map/1]).
-import(scran_result, [kv/2]).
-import(scran_sequence, [combined_with/2]).
-import(scran_sequence, [sequence/1]).


-spec decode() -> scran:parser(binary(), msmp:handshake()).

decode() ->
    fun
        (Input) ->
            (combined_with(
               into_map(
                 sequence(
                   [kv(action, value(handshake, tag(<<10:8>>))),
                    kv(server_version, null_terminated()),
                    kv(thread_id, msmp_integer_fixed:decode(4)),
                    kv(auth_plugin_data_part_1, take(8)),
                    kv(filler, msmp_integer_fixed:decode(1)),
                    kv(capability_flags_1, msmp_capabilities:decode(lower)),
                    kv(character_set, msmp_integer_fixed:decode(1)),
                    kv(status_flags, msmp_integer_fixed:decode(2)),
                    kv(capability_flags_2, msmp_capabilities:decode(upper)),
                    kv(auth_plugin_data_len, msmp_integer_fixed:decode(1)),
                    alt([sequence(
                           [kv(reserved, tag(<<0:10/unit:8>>))]),

                         sequence(
                           [kv(reserved, tag(<<0:6/unit:8>>)),
                            kv(capability_flags_3, msmp_integer_fixed:decode(4))])])])),
               fun
                   (#{capability_flags_2 := #{plugin_auth := true},
                      auth_plugin_data_len := AuthPluginDataLen}) ->
                       into_map(
                         sequence(
                           [kv(auth_plugin_data_part_2,
                               take(max(13, AuthPluginDataLen - 8))),
                            kv(auth_plugin_name,
                               into_atom(null_terminated()))]))
               end))(Input)
    end.
