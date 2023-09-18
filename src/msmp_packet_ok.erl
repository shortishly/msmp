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


-module(msmp_packet_ok).


-feature(maybe_expr, enable).

-export([decode/1]).
-import(scran_bytes, [tag/1]).
-import(scran_combinator, [condition/2]).
-import(scran_combinator, [condition/3]).
-import(scran_combinator, [value/2]).
-import(scran_result, [into_map/1]).
-import(scran_result, [kv/2]).
-import(scran_sequence, [combined_with/2]).
-import(scran_sequence, [sequence/1]).
-include_lib("kernel/include/logger.hrl").


decode(ClientFlags) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{client_flags => ClientFlags, input => Input}),

            (combined_with(
               into_map(
                 sequence(
                   [kv(action, value(ok, tag(<<16#00>>))),
                    kv(affected_rows, msmp_integer_variable:decode()),
                    kv(last_insert_id, msmp_integer_variable:decode()),

                    condition(
                      fun
                          () ->
                              maps:get(protocol_41, ClientFlags)
                      end,
                      sequence(
                        [kv(status_flags, msmp_server_status:decode()),
                         kv(warnings, msmp_integer_fixed:decode(1))]),

                      condition(
                        maps:get(transactions, ClientFlags),
                        kv(status_flags, msmp_server_status:decode())))])),
               fun
                   (#{status_flags := #{session_state_changed := StateChanged}}) ->
                       into_map(
                         sequence(
                           [condition(
                              fun
                                  () ->
                                      maps:get(session_track, ClientFlags)
                              end,

                              sequence(
                                [kv(info, msmp_string_length_encoded:decode()),
                                 condition(
                                   StateChanged,
                                   kv(session_state_info, msmp_string_length_encoded:decode()))]),

                              sequence(
                                [kv(info, msmp_string_rest_of_packet:decode())]))]))
               end))(Input)
    end.
