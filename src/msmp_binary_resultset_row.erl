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


-module(msmp_binary_resultset_row).


-export([decode/1]).
-include_lib("kernel/include/logger.hrl").


packet_header() ->
    <<0>>.


decode(Definitions) ->
    fun
        (Encoded) ->
            ?LOG_DEBUG(#{encoded => Encoded}),

            (scran_sequence:combined_with(
               scran_result:into_map(
                 scran_sequence:sequence(
                   [scran_result:kv(
                      action,
                      scran_combinator:value(
                        binary_resultset_row,
                        scran_bytes:tag(packet_header()))),
                    scran_result:kv(
                      null_bitmap,
                      msmp_null_bitmap:decode(length(Definitions), 2))])),
               fun
                   (#{null_bitmap := Nulls}) ->
                       scran_result:into_map(
                         scran_sequence:sequence(
                           [scran_result:kv(
                              row,
                              scran_sequence:sequence(
                                lists:filtermap(
                                  fun
                                      ({Definition, false}) ->
                                          {true, msmp_binary:decode(Definition)};

                                      ({_, true}) ->
                                          {true, scran_combinator:success(null)}
                                  end,
                                  lists:zip(Definitions, Nulls))))]))
               end))(Encoded)
    end.
