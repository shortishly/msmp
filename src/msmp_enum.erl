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


-module(msmp_enum).


-export([into_map/1]).
-export([priv_consult/1]).


priv_consult(Filename) ->
    into_map(msmp:priv_consult(Filename)).


-spec into_map([atom() | {atom(), integer()}]) -> #{atom() | integer() => integer() | atom()}.

into_map(Enumeration) ->
    maps:from_list(
      lists:flatten(
        element(
          1,
          lists:mapfoldl(
            fun
                ({Name, Value}, _) ->
                    {kv_vk(Name, Value), Value + 1};

                (Name, Value) ->
                    {kv_vk(Name, Value), Value + 1}
            end,
            0,
            Enumeration)))).


kv_vk(K, V) ->
    [{K, V}, {V, K}].
