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


-module(msmp_codec).


-feature(maybe_expr, enable).


-export([decode/1]).
-export([encode/0]).
-export([encode/1]).
-include_lib("kernel/include/logger.hrl").


decode(Parser) ->
    fun
        (Input) ->
            (scran_result:into_map(
               scran_combinator:map_parser(
                 scran_bytes:length_encoded(
                   scran_combinator:map_result(
                     msmp_integer_fixed:decode(3),
                     fun
                         (Length) ->
                             %% adjust length to include the sequence number
                             Length + 1
                     end)),
                 scran_sequence:sequence(
                   [scran_result:kv(sequence, msmp_integer_fixed:decode(1)),
                    scran_result:kv(packet, Parser)]))))(Input)
    end.


encode() ->
    ?FUNCTION_NAME(
       narcs_branch:alt(
         [msmp_handshake_response:encode(),
          msmp_com_query:encode()])).


encode(Parser) ->
    fun
        (Decoded) ->
            ?LOG_DEBUG(#{decoded => Decoded}),
            (narcs_combinator:map_result(
               narcs_sequence:sequence(
                 [narcs_combinator:v(
                    sequence,
                    msmp_integer_fixed:encode(1)),
                  narcs_combinator:v(
                    packet,
                    Parser)]),
               narcs_bytes:length_encoded(
                 fun
                     (Length) ->
                         %% adjust length to exclude the sequence number
                         (msmp_integer_fixed:encode(3))(Length - 1)
                 end)))(Decoded)
    end.
