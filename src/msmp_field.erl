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


-module(msmp_field).


-define(TIMEF_INT_OFS, 16#80_00_00).
-export([is_character_type/1]).
-export([is_numeric_type/1]).
-export([lookup/1]).
-export([types/0]).
-export_type([type/0]).
-include_lib("kernel/include/logger.hrl").
-on_load(on_load/0).


-type type() :: long
              | longlong
              | tiny
              | short
              | int24
              | enum
              | string
              | varchar
              | date
              | time
              | time2
              | timestamp2
              | year
              | datetime
              | datetime2
              | float
              | double
              | bit.


is_character_type(Type) ->
    lists:member(Type, [string, var_string, varchar, blob]).


is_numeric_type(Type) ->
    lists:member(
      Type,
      [tiny,
       short,
       int24,
       long,
       longlong,
       year,
       float,
       double,
       decimal,
       newdecimal]).


on_load() ->
    persistent_term:put(
      ?MODULE,
      msmp_enum:priv_consult("field-type.terms")).


types() ->
    persistent_term:get(?MODULE).


lookup(FieldType) ->
    maps:get(FieldType, types()).
