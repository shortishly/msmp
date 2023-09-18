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


-module(msmp_auth_switch_request).


-export([decode/0]).


-spec decode() -> scran:parser(binary(), msmp:auth_switch_request()).

decode() ->
    fun
        (Input) ->
            (scran_result:into_map(
               scran_sequence:sequence(
                 [scran_result:kv(
                    action,
                    scran_combinator:value(
                      auth_switch_request,
                      scran_bytes:tag(<<254>>))),

                  scran_result:kv(
                    plugin_name,
                    scran_result:into_atom(
                      msmp_string_null_terminated:decode())),

                  scran_result:kv(
                    plugin_provided_data,
                    scran_combinator:rest())])))(Input)
    end.
