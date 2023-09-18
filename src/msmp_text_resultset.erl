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


-module(msmp_text_resultset).


-feature(maybe_expr, enable).


-export([decode/0]).
-include_lib("kernel/include/logger.hrl").


decode() ->
    fun
        (Encoded) ->
            (scran_result:into_map(
               scran_sequence:sequence(
                 [scran_result:kv(
                    row,
                    scran_multi:many1(msmp_string_length_encoded:decode())),

                  scran_result:kv(
                    action,
                    scran_combinator:value(
                      text_resultset,
                      scran_combinator:eof()))])))(Encoded)
    end.
