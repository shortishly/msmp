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


-module(msmp_column_definition_flags).


-export([decode/1]).
-export([encode/1]).
-include_lib("kernel/include/logger.hrl").
-on_load(on_load/0).


decode(Flags) ->
    maps:map(
      fun
          (_, Mask) ->
              Flags band Mask == Mask
      end,
      flags()).


encode(Flags) ->
    lists:foldl(
      fun
          (Flag, A) ->
              {ok, Mask} = maps:find(Flag, flags()),
              A bor Mask
      end,
      0,
      Flags).


flags() ->
    persistent_term:get(?MODULE).


on_load() ->
    persistent_term:put(
      ?MODULE,
      maps:from_list(
        msmp:priv_consult("column-definition-flag.terms"))).
