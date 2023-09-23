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


-module(msmp_field_optional_metadata).


-feature(maybe_expr, enable).


-export([decode/1]).
-export_type([metadata/0]).
-export_type([tag/0]).
-import(scran_bits, [into_boolean/0]).
-import(scran_bytes, [take/1]).
-import(scran_combinator, [map_parser/2]).
-import(scran_combinator, [map_result/2]).
-import(scran_combinator, [rest/0]).
-import(scran_multi, [count/2]).
-import(scran_multi, [many1/1]).
-import(scran_result, [into_map/1]).
-import(scran_result, [kv/2]).
-import(scran_sequence, [followed_with/2]).
-import(scran_sequence, [terminated/2]).
-include_lib("kernel/include/logger.hrl").
-on_load(on_load/0).


-type tag() :: unsignedness
             | simple_primary_key
             | column_charset
             | geometry_type
             | emum_and_set_column_charset
             | default_charset
             | column_name.

-type metadata() :: #{unsignedness => #{non_neg_integer() := boolean()},
                      default_charset => [non_neg_integer()],
                      column_name => [binary()],
                      simple_primary_key => [non_neg_integer()],
                      column_visibility => binary()}.

-spec decode([msmp_field:type()]) -> scran:parser(binary(), metadata()).

decode(ColTypes) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{col_types => ColTypes, input => Input}),
            (into_map(many1(tlv(ColTypes))))(Input)
    end.


tlv(ColTypes) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{col_types => ColTypes, input => Input}),

            (followed_with(
               map_result(
                 msmp_integer_fixed:decode(1),
                 fun lookup/1),
               fun
                   (Tag) ->
                       map_parser(
                         take(msmp_integer_variable:decode()),
                         kv(Tag, value(Tag, ColTypes)))
               end))(Input)
    end.


value(unsignedness = Tag, ColTypes) ->
    NumericColPositions = numeric_col_positions(ColTypes),

    fun
        (Input) ->
            ?LOG_DEBUG(#{col_types => ColTypes,
                         tag => Tag,
                         numeric_col_positions => NumericColPositions,
                         input => Input}),

            (into_map(
               map_result(
                 terminated(
                   count(length(NumericColPositions), into_boolean()),
                   scran_bits:tag(padding(length(NumericColPositions)))),
                 fun
                     (Flags) ->
                         ?LOG_DEBUG(#{flags => Flags,
                                      numeric_col_positions => NumericColPositions}),

                         lists:zip(NumericColPositions, Flags)
                 end)))(Input)
    end;

value(Tag, ColTypes)
  when Tag == simple_primary_key;
       Tag == column_charset;
       Tag == geometry_type;
       Tag == emum_and_set_column_charset;
       Tag == default_charset ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{tag => Tag,
                         col_types => ColTypes,
                         input => Input}),
            (many1(msmp_integer_variable:decode()))(Input)
    end;

value(column_name = Tag, ColTypes) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{tag => Tag,
                         col_types => ColTypes,
                         input => Input}),
            (many1(msmp_string_length_encoded:decode()))(Input)
    end;

value(enum_str_value = Tag, ColTypes) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{tag => Tag,
                         col_types => ColTypes,
                         input => Input}),
            (many1(
               count(
                 msmp_integer_variable:decode(),
                 msmp_string_length_encoded:decode())))(Input)
    end;

value(Tag, ColTypes) ->
    %% TODO remove this catch all for unhandled tlv
    fun
        (Input) ->
            ?LOG_DEBUG(#{col_types => ColTypes,
                         tag => Tag,
                         input => Input}),

            (rest())(Input)
    end.


padding(N) when N rem 8 == 0 ->
    <<>>;

padding(N) ->
    <<0:(8 - N rem 8)>>.


numeric_col_positions(ColTypes) ->
    lists:filtermap(
      fun
          ({Position, Type}) ->
              case msmp_field:is_numeric_type(Type) of
                  true ->
                      {true, Position};

                  false ->
                      false
              end
      end,
      pos_col_type(ColTypes)).


pos_col_type(ColTypes) ->
    lists:zip(lists:seq(1, length(ColTypes)), ColTypes).


on_load() ->
    persistent_term:put(
      ?MODULE,
      msmp_enum:priv_consult("optional-metadata-field-type.terms")).


lookup(FieldType) ->
    maps:get(FieldType, types()).


types() ->
    persistent_term:get(?MODULE).
