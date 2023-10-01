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


-module(msmp_com_query).


-feature(maybe_expr, enable).


-export([decode/1]).
-export([encode/1]).


decode(ClientFlags) ->
    fun
        (Input) ->
            (scran_result:into_map(
               scran_sequence:sequence(
                 [scran_result:kv(
                    action,
                    scran_combinator:value(
                      com_query,
                      scran_bytes:tag(command()))),

                  scran_combinator:condition(
                    fun
                        () ->
                            maps:get(query_attributes, ClientFlags)
                    end,
                    scran_sequence:sequence(
                      [scran_result:kv(
                         parameter_count,
                         msmp_integer_fixed:decode(1)),

                       scran_branch:alt(
                         [scran_sequence:sequence(
                            [scran_combinator:ignore_result(
                               scran_combinator:peek(
                                 scran_bytes:tag(<<1:8>>))),

                             scran_result:kv(
                               parameter_set,
                               msmp_integer_fixed:decode(1)),

                             scran_result:kv(
                               query,
                               scran_combinator:rest())]),

                          scran_sequence:sequence(
                            [scran_result:kv(
                               parameter_set,
                               msmp_integer_fixed:decode(1)),

                             scran_result:kv(
                               unknown_uint4,
                               msmp_integer_fixed:decode(4)),

                             scran_result:kv(
                               query,
                               msmp_string_length_encoded:decode()),

                             scran_result:kv(
                               rest,
                               scran_combinator:rest())])])]),

                    scran_sequence:sequence(
                     [scran_result:kv(
                        query,
                        scran_combinator:rest())]))])))(Input)
    end.


encode(#{query_attributes := QueryAttributes}) ->
    fun
        (Decoded) ->
            (narcs_sequence:sequence(
               [narcs_bytes:tag(command()),
                narcs_combinator:condition(
                  QueryAttributes,
                  narcs_sequence:sequence(
                    [narcs_combinator:v(
                       parameter_count,
                       msmp_integer_fixed:encode(1)),
                     narcs_combinator:v(
                       parameter_set,
                       msmp_integer_fixed:encode(1))])),
                narcs_combinator:v(
                  query,
                  narcs_combinator:rest())]))(Decoded)
    end.


command() ->
    <<3>>.
