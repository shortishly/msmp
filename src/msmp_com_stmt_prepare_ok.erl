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


-module(msmp_com_stmt_prepare_ok).


-export([decode/1]).


decode(#{optional_resultset_metadata := Optional}) ->
    fun
        (Encoded) ->
            (scran_result:into_map(
               scran_sequence:sequence(
                 [scran_result:kv(
                    action,
                    scran_combinator:value(
                      com_stmt_prepare_ok,
                      scran_bytes:tag(<<0>>))),

                  scran_result:kv(
                    statement_id,
                    msmp_integer_fixed:decode(4)),

                  scran_result:kv(
                    num_columns,
                    msmp_integer_fixed:decode(2)),

                  scran_result:kv(
                    num_params,
                    msmp_integer_fixed:decode(2)),

                  scran_result:kv(
                    reserved_1,
                    msmp_integer_fixed:decode(1)),

                  scran_combinator:opt(
                    scran_sequence:sequence(
                      [scran_result:kv(
                         warning_count,
                         msmp_integer_fixed:decode(2)),
                       scran_combinator:condition(
                         Optional,
                         scran_result:kv(
                           metadata_follows,
                           msmp_integer_fixed:decode(1)))]))])))(Encoded)
    end.
