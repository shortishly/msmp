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


-module(msmp_com_stmt_execute).


-export([encode/1]).


command() ->
    <<16#17>>.


encode(#{query_attributes := QueryAttributes}) ->
    fun
        (Decoded) ->
            (narcs_sequence:sequence(
               [narcs_bytes:tag(command()),
                narcs_combinator:v(
                  statement_id,
                  msmp_integer_fixed:encode(4)),
                narcs_combinator:v(
                  flags,
                  msmp_integer_fixed:encode(1)),
                narcs_combinator:v(
                  iteration_count,
                  msmp_integer_fixed:encode(4)),
                narcs_combinator:condition(
                  QueryAttributes,
                  narcs_combinator:v(
                    parameters,
                    narcs_combinator:map_result(
                      fun
                          (Parameters) ->
                              length(Parameters)
                      end,
                      msmp_integer_variable:encode()))),
                narcs_combinator:v(
                  parameters,
                  msmp_null_bitmap:encode()),
                narcs_combinator:v(
                  new_params_bind_flag,
                  msmp_integer_fixed:encode(1)),
                types(),
                narcs_combinator:condition(
                  QueryAttributes,
                  names()),
                values()]))(Decoded)
    end.


types() ->
    fun
        (#{types := Types, parameters := Parameters}) ->
            lists:map(
              fun
                  ({#{type := Type, flags := #{unsigned := Unsigned}}, Value}) ->
                      [msmp_field:lookup(type(Type, Value)),
                       case Unsigned of
                           true ->
                               16#80;

                           false ->
                               0
                       end]
              end,
              lists:zip(Types, Parameters))
    end.


names() ->
    fun
        (#{types := Types}) ->
            lists:map(
              fun
                  (#{name := Name}) ->
                      (msmp_string_length_encoded:encode())(Name)
              end,
              Types)
    end.


values() ->
    fun
        (#{types := Types, parameters := Parameters}) ->
            lists:filtermap(
              fun
                  ({_, null}) ->
                      false;

                  ({Definition, Parameter}) ->
                      {true,
                       (msmp_binary:encode(
                          type(Definition, Parameter)))
                         (Parameter)}
              end,
              lists:zip(Types, Parameters))
    end.


type(#{type := null = Type} = Definition, Parameter) ->
    Definition#{type := type(Type, Parameter)};

type(#{type := _} = Definition, _) ->
    Definition;

type(null, Parameter) when is_integer(Parameter) ->
    longlong;

type(null, Parameter) when is_float(Parameter) ->
    double;

type(null, Parameter) when is_binary(Parameter) ->
    varchar;

type(Type, _) when is_atom(Type) ->
    Type.
