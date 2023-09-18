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


-module(msmp_packet_eof).


-export([decode/1]).
-import(scran_bytes, [tag/1]).
-import(scran_combinator, [condition/2]).
-import(scran_combinator, [value/2]).
-import(scran_result, [into_map/1]).
-import(scran_result, [kv/2]).
-import(scran_sequence, [sequence/1]).


decode(#{protocol_41 := Protocol41}) ->
    fun
        %% An EOF packet must be smaller than 9 bytes.
        (Input) when byte_size(Input) < 9 ->
            (into_map(
               sequence(
                 [kv(action, value(eof, tag(<<16#fe>>))),
                  condition(
                    Protocol41,
                    sequence(
                     [kv(warnings, msmp_integer_fixed:decode(2)),
                      kv(status_flags, msmp_server_status:decode())]))])))(Input);

        (_) ->
            nomatch
    end.
