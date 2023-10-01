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


-module(msmp_column_definition).


-feature(maybe_expr, enable).


-export([decode/0]).
-include_lib("kernel/include/logger.hrl").


decode() ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{input => Input}),

            (scran_result:into_map(
                 scran_sequence:sequence(
                   [scran_result:kv(
                      catalog,
                      msmp_string_length_encoded:decode()),
                    scran_result:kv(
                      schema,
                      msmp_string_length_encoded:decode()),
                    scran_result:kv(
                      table,
                      msmp_string_length_encoded:decode()),
                    scran_result:kv(
                      org_table,
                      msmp_string_length_encoded:decode()),
                    scran_result:kv(
                      name,
                      msmp_string_length_encoded:decode()),
                    scran_result:kv(
                      org_name,
                      msmp_string_length_encoded:decode()),
                    scran_result:kv(
                      length_of_fixed_length_fields,
                      msmp_integer_variable:decode()),
                    scran_result:kv(
                      character_set,
                      msmp_integer_fixed:decode(2)),
                    scran_result:kv(
                      column_length,
                      msmp_integer_fixed:decode(4)),
                    scran_result:kv(
                      type,
                      scran_combinator:map_result(
                        msmp_integer_fixed:decode(1),
                        fun msmp_field:lookup/1)),
                    scran_result:kv(
                      flags,
                      scran_combinator:map_result(
                        msmp_integer_fixed:decode(2),
                        fun msmp_column_definition_flags:decode/1)),
                    scran_result:kv(
                      decimals,
                      msmp_integer_fixed:decode(1)),
                    scran_result:kv(
                      reserved0,
                      scran_bytes:take(2)),
                    scran_result:kv(
                      action,
                      scran_combinator:value(
                        column_definition,
                        scran_combinator:eof()))])))(Input)
    end.
