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


-module(msmp_handshake_response).


-feature(maybe_expr, enable).


-export([auth_response/2]).
-export([decode/0]).
-export([encode/0]).


-spec decode() -> scran:parser(binary(), msmp:handshake_response()).

decode() ->
    fun
        (Input) ->
            (scran_sequence:combined_with(
               scran_result:into_map(
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

                    scran_branch:alt(
                      [scran_result:kv(
                         action,
                         scran_combinator:value(
                           handshake_response,
                           scran_bytes:tag(<<0:23/unit:8>>))),

                       scran_sequence:sequence(
                         [scran_result:kv(
                            action,
                            scran_combinator:value(
                              handshake_response,
                              scran_bytes:tag(<<0:19/unit:8>>))),

                          scran_result:kv(
                            extended_capabilities,
                            msmp_integer_fixed:decode(4))])]),

                    scran_result:kv(
                      username,
                      msmp_string_null_terminated:decode())])),
               fun
                   (#{client_flags := #{plugin_auth_lenenc_client_data := LengthEncoded,
                                        connect_with_db := ConnectWithDB,
                                        plugin_auth := PluginAuth,
                                        connect_attrs := ConnectAttrs,
                                        zstd_compression := ZStdCompression}}) ->
                       scran_result:into_map(
                         scran_sequence:sequence(
                           [scran_combinator:condition(
                              LengthEncoded,
                              scran_result:kv(
                                auth_response,
                                msmp_string_length_encoded:decode())),

                            scran_combinator:condition(
                              ConnectWithDB,
                              scran_result:kv(
                                database,
                                msmp_string_null_terminated:decode())),

                            scran_combinator:condition(
                              PluginAuth,
                              scran_result:kv(
                                client_plugin_name,
                                scran_result:into_atom(
                                  msmp_string_null_terminated:decode()))),

                            scran_combinator:condition(
                              ConnectAttrs,
                              scran_result:kv(
                                connect_attrs,
                                scran_result:into_map(
                                  scran_combinator:map_parser(
                                    scran_bytes:take(msmp_integer_variable:decode()),
                                    scran_multi:many1(
                                      scran_result:into_tuple(
                                        scran_sequence:sequence(
                                          [msmp_string_length_encoded:decode(),
                                           msmp_string_length_encoded:decode()]))))))),

                            scran_combinator:condition(
                              ZStdCompression,
                              scran_result:kv(
                                zstd_compression_level,
                                msmp_integer_fixed:decode(1)))]))
               end))(Input)
    end.


encode() ->
    fun
        (#{action := handshake_response,
           extended_capabilities := _,
           client_flags := #{plugin_auth_lenenc_client_data := LengthEncoded,
                             connect_with_db := ConnectWithDB,
                             plugin_auth := PluginAuth,
                             connect_attrs := ConnectAttrs,
                             zstd_compression := ZStdCompression}} = Decoded) ->
            (narcs_sequence:sequence(
               [narcs_combinator:v(
                  client_flags,
                  msmp_capabilities:encode(combined)),
                narcs_combinator:v(
                  max_packet_size,
                  msmp_integer_fixed:encode(4)),
                narcs_combinator:v(
                  character_set,
                  msmp_integer_fixed:encode(1)),
                narcs_bytes:tag(<<0:19/unit:8>>),
                narcs_combinator:v(
                  extended_capabilities,
                  msmp_integer_fixed:encode(4)),
                narcs_combinator:v(
                  username,
                  msmp_string_null_terminated:encode()),

                narcs_combinator:condition(
                  LengthEncoded,
                  narcs_combinator:v(
                    auth_response,
                    msmp_string_length_encoded:encode())),

               narcs_combinator:condition(
                 ConnectWithDB,
                 narcs_combinator:v(
                   database,
                   msmp_string_null_terminated:encode())),

               narcs_combinator:condition(
                 PluginAuth,
                 narcs_combinator:v(
                   client_plugin_name,
                   narcs_combinator:map_result(
                     narcs_bytes:from_atom(),
                     narcs_bytes:null_terminated()))),

                narcs_combinator:condition(
                  ConnectAttrs,
                  narcs_combinator:v(
                    connect_attrs,
                    narcs_combinator:map_result(
                      narcs_sequence:sequence(
                        msmp_string_length_encoded:encode(),
                        msmp_string_length_encoded:encode()),
                      narcs_bytes:length_encoded(
                        msmp_integer_variable:encode())))),

                narcs_combinator:condition(
                  ZStdCompression,
                  narcs_combinator:v(
                    zstd_compression_level,
                    msmp_integer_fixed:encode(1)))]))(Decoded);


        (#{action := handshake_response,
           client_flags := #{plugin_auth_lenenc_client_data := LengthEncoded,
                             connect_with_db := ConnectWithDB,
                             plugin_auth := PluginAuth,
                             connect_attrs := ConnectAttrs,
                             zstd_compression := ZStdCompression}} = Decoded) ->
            (narcs_sequence:sequence(
               [narcs_combinator:v(
                  client_flags,
                  msmp_capabilities:encode(combined)),
                narcs_combinator:v(
                  max_packet_size,
                  msmp_integer_fixed:encode(4)),
                narcs_combinator:v(
                  character_set,
                  msmp_integer_fixed:encode(1)),
                narcs_bytes:tag(<<0:23/unit:8>>),
                narcs_combinator:v(
                  username,
                  msmp_string_null_terminated:encode()),

                narcs_combinator:condition(
                  LengthEncoded,
                  narcs_combinator:v(
                    auth_response,
                    msmp_string_length_encoded:encode())),

               narcs_combinator:condition(
                 ConnectWithDB,
                 narcs_combinator:v(
                   database,
                   msmp_string_null_terminated:encode())),

               narcs_combinator:condition(
                 PluginAuth,
                 narcs_combinator:v(
                   client_plugin_name,
                   narcs_combinator:map_result(
                     narcs_bytes:from_atom(),
                     narcs_bytes:null_terminated()))),

                narcs_combinator:condition(
                  ConnectAttrs,
                  narcs_combinator:v(
                    connect_attrs,
                    narcs_combinator:map_result(
                      narcs_sequence:sequence(
                        msmp_string_length_encoded:encode(),
                        msmp_string_length_encoded:encode()),
                      narcs_bytes:length_encoded(
                        msmp_integer_variable:encode())))),

                narcs_combinator:condition(
                  ZStdCompression,
                  narcs_combinator:v(
                    zstd_compression_level,
                    msmp_integer_fixed:encode(1)))]))(Decoded);
        (_) ->
            nomatch
    end.


auth_response(#{auth_plugin_name := mysql_native_password,
                auth_plugin_data_part_1 := <<AuthDataPartOne:8/bytes>>,
                auth_plugin_data_part_2 := <<AuthDataPartTwo:12/bytes, 0>>},
              Password) ->
    mysql_native_password([AuthDataPartOne, AuthDataPartTwo], Password);

auth_response(#{auth_plugin_name := caching_sha2_password,
                auth_plugin_data_part_1 := <<AuthDataPartOne:8/bytes>>,
                auth_plugin_data_part_2 := <<AuthDataPartTwo:12/bytes, 0>>},
              Password) ->
    caching_sha2_password([AuthDataPartOne, AuthDataPartTwo], Password);

auth_response(#{plugin_name := mysql_native_password,
                plugin_provided_data := <<PluginProvidedData:20/bytes, 0>>},
              Password) ->
    mysql_native_password(PluginProvidedData, Password);

auth_response(#{plugin_name := caching_sha2_password,
                plugin_provided_data := <<PluginProvidedData:20/bytes, 0>>},
              Password) ->
    mysql_native_password(PluginProvidedData, Password).


mysql_native_password(PluginProvidedData, Password) ->
    crypto:exor(
      crypto:hash(sha, Password),
      crypto:hash(sha,
                  [PluginProvidedData,
                   crypto:hash(
                     sha,
                     crypto:hash(
                       sha,
                       Password))])).

caching_sha2_password(PluginProvidedData, Password) ->
    crypto:exor(
      crypto:hash(sha256, Password),
      crypto:hash(sha256,
                  [crypto:hash(sha256, crypto:hash(sha256, Password)),
                   PluginProvidedData])).
