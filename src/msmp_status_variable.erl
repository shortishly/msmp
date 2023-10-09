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


-module(msmp_status_variable).


-feature(maybe_expr, enable).


-export([decode/0]).
-import(scran_branch, [alt/1]).
-import(scran_bytes, [tag/1]).
-import(scran_bytes, [take/1]).
-import(scran_combinator, [rest/0]).
-import(scran_multi, [many1/1]).
-import(scran_result, [into_map/1]).
-import(scran_result, [kv/2]).
-import(scran_sequence, [preceded/2]).
-import(scran_sequence, [sequence/1]).
-include_lib("kernel/include/logger.hrl").
-on_load(on_load/0).


decode() ->
    fun
        (<<>>) ->
            {<<>>, #{}};

        (Input) ->
            (variables(
               alt([flags2(),
                    sql_mode(),
                    catalog_nz(),
                    charset(),

                    table_map_for_update(),

                    ddl_xid(),
                    default_collation_for_utf8mb4(),

                    %% mariadb
                    xid(),

                    %% catch all
                    unknown()])))(Input)
    end.


variables(Parser) ->
    fun
        (Input) ->
            (into_map(many1(Parser)))(Input)
    end.


flags2() ->
    fun
        (Input) ->
            (variable(?FUNCTION_NAME, take(4)))(Input)
    end.


sql_mode() ->
    fun
        (Input) ->
            (variable(?FUNCTION_NAME, take(8)))(Input)
    end.


catalog_nz() ->
    fun
        (Input) ->
            (variable(?FUNCTION_NAME, take(msmp_integer_fixed:decode(1))))(Input)
    end.


ddl_xid() ->
    fun
        (Input) ->
            (variable(?FUNCTION_NAME, msmp_integer_fixed:decode(8)))(Input)
    end.

table_map_for_update() ->
    fun
        (Input) ->
            (variable(?FUNCTION_NAME, msmp_integer_fixed:decode(8)))(Input)
    end.


default_collation_for_utf8mb4() ->
    fun
        (Input) ->
            (variable(?FUNCTION_NAME, msmp_integer_fixed:decode(2)))(Input)
    end.


xid() ->
    fun
        (Input) ->
            (variable(?FUNCTION_NAME, msmp_integer_fixed:decode(8)))(Input)
    end.


unknown() ->
    fun
        (Input) ->
            (kv(?FUNCTION_NAME,
                into_map(
                  sequence(
                    [kv(type, msmp_integer_fixed:decode(1)),
                     kv(data, rest())]))))(Input)
    end.


charset() ->
    fun
        (Input) ->
            (variable(
               ?FUNCTION_NAME,
               into_map(
                 sequence(
                   [kv(character_set_client, msmp_integer_fixed:decode(2)),
                    kv(collation_connection, msmp_integer_fixed:decode(2)),
                    kv(collation_server, msmp_integer_fixed:decode(2))]))))(Input)
    end.


variable(Name, Parser) ->
    fun
        (Input) ->
            (kv(Name, preceded(type(Name), Parser)))(Input)
    end.


type(Type) ->
    fun
        (Input) ->
            (tag(<<(cache(Type)):8>>))(Input)
    end.


cache(Type) ->
    maps:get(Type, persistent_term:get(?MODULE)).


on_load() ->
    persistent_term:put(
      ?MODULE,
      msmp_enum:priv_consult("status-variable-type.terms")).
