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


-module(msmp_gtid_set).


-export([decode/0]).
-export([encode/0]).


decode() ->
    fun
        (Encoded) ->
            (scran_multi:count(
               msmp_integer_fixed:decode(8),
               scran_result:into_map(
                 scran_sequence:sequence(
                   [scran_result:kv(
                      sid,
                      scran_bytes:take(16)),
                    scran_result:kv(
                      intervals,
                      scran_multi:count(
                        msmp_integer_fixed:decode(8),
                        scran_result:into_map(
                          scran_sequence:sequence(
                            [scran_result:kv(
                               start,
                               msmp_integer_fixed:decode(8)),
                             scran_result:kv(
                               finish,
                               msmp_integer_fixed:decode(8))]))))]))))(Encoded)
    end.


encode() ->
    fun
        (Decoded) ->
            (narcs_multi:count(
               msmp_integer_fixed:encode(8),
               narcs_sequence:sequence(
                 [narcs_combinator:v(sid, narcs_bytes:take(16)),
                  narcs_combinator:v(
                    intervals,
                    narcs_multi:count(
                      msmp_integer_fixed:encode(8),
                      narcs_sequence:sequence(
                        [narcs_combinator:v(
                           start,
                           msmp_integer_fixed:encode(8)),

                         narcs_combinator:v(
                           finish,
                           msmp_integer_fixed:encode(8))])))])))(Decoded)
    end.
