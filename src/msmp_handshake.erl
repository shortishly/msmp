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


-spec decode() -> scran:parser(binary(), msmp:handshake()).

decode() ->
    fun
        (Input) ->
            (scran_sequence:combined_with(
               scran_result:into_map(
                 scran_sequence:sequence(
                   [scran_result:kv(
                      action,
                      scran_combinator:value(
                        handshake,
                        scran_bytes:tag(<<10:8>>))),

                    scran_result:kv(
                      server_version,
                      scran_bytes:null_terminated()),

                    scran_result:kv(
                      thread_id,
                      msmp_integer_fixed:decode(4)),

                    scran_result:kv(
                      auth_plugin_data_part_1,
                      scran_bytes:take(8)),

                    scran_result:kv(
                      filler,
                      msmp_integer_fixed:decode(1)),

                    scran_result:kv(
                      capability_flags_1,
                      msmp_capabilities:decode(lower)),

                    scran_result:kv(
                      character_set,
                      msmp_integer_fixed:decode(1)),

                    scran_result:kv(
                      status_flags,
                      msmp_integer_fixed:decode(2)),

                    scran_result:kv(
                      capability_flags_2,
                      msmp_capabilities:decode(upper)),

                    scran_result:kv(
                      auth_plugin_data_len,
                      msmp_integer_fixed:decode(1)),

                    %% Determine whether we are connected to MySQL or
                    %% MariaDB
                    %%
                    scran_branch:alt(
                      [scran_sequence:sequence(
                         [scran_result:kv(
                            reserved,
                            scran_bytes:tag(<<0:10/unit:8>>)),

                         scran_result:kv(
                           operator,
                          scran_combinator:success(mysql))]),

                       scran_sequence:sequence(
                         [scran_result:kv(
                            reserved,
                            scran_bytes:tag(<<0:6/unit:8>>)),

                          scran_result:kv(
                            extended_capabilities,
                            msmp_integer_fixed:decode(4)),

                          scran_result:kv(
                            operator,
                            scran_combinator:success(mariadb))])])])),
               fun
                   (#{capability_flags_2 := #{plugin_auth := true},
                      auth_plugin_data_len := AuthPluginDataLen}) ->
                       scran_result:into_map(
                         scran_sequence:sequence(
                           [scran_result:kv(
                              auth_plugin_data_part_2,
                              scran_bytes:take(max(13, AuthPluginDataLen - 8))),

                            scran_result:kv(
                              auth_plugin_name,
                              scran_result:into_atom(
                                scran_bytes:null_terminated()))]))
               end))(Input)
    end.
