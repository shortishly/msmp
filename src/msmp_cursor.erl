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


-module(msmp_cursor).


-export([lookup/1]).
-export([types/0]).
-export_type([type/0]).
-on_load(on_load/0).

-type type() :: no_cursor
              | read_only
              | for_update
              | scrollable.


on_load() ->
    persistent_term:put(
      ?MODULE,
      msmp_enum:priv_consult("cursor.terms")).


types() ->
    persistent_term:get(?MODULE).


lookup(Type) ->
    maps:get(Type, types()).
