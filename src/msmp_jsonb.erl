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


-module(msmp_jsonb).


-feature(maybe_expr, enable).


-export([decode/1]).
-export([type/1]).
-import(scran_branch, [alt/1]).
-import(scran_bytes, [tag/1]).
-import(scran_bytes, [take/1]).
-import(scran_combinator, [map_parser/2]).
-import(scran_combinator, [map_result/2]).
-import(scran_combinator, [condition/2]).
-import(scran_multi, [count/2]).
-import(scran_result, [into_map/1]).
-import(scran_result, [into_tuple/1]).
-import(scran_result, [kv/2]).
-import(scran_sequence, [followed_with/2]).
-import(scran_sequence, [preceded/2]).
-import(scran_sequence, [sequence/1]).
-import(scran_sequence, [zip/2]).
-include_lib("kernel/include/logger.hrl").
-on_load(on_load/0).


decode(Metadata) ->
    fun
        (Input) ->
            ?LOG_DEBUG(
               #{metadata => Metadata,
                 input =>Input}),

            (map_parser(
               take(msmp_integer_fixed:decode(Metadata)),
               alt([object(),

                    preceded(
                      type(string),
                      msmp_string_length_encoded:decode()),

                    numeric(),

                    literal(single),

                    array()])))(Input)
    end.


numeric() ->
    fun
        (Input) ->
            (alt([preceded(type(int16), scran_number:i(little, 16)),
                  preceded(type(uint16), scran_number:u(little, 16)),

                  preceded(type(int32), scran_number:i(little, 32)),
                  preceded(type(uint32), scran_number:u(little, 32)),

                  preceded(type(int64), scran_number:i(little, 64)),
                  preceded(type(uint64), scran_number:u(little, 64)),

                  preceded(type(double), scran_number:f(little, 64))]))(Input)
    end.


array() ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{input => Input}),
            (alt([array(small),
                  array(large)]))(Input)
    end.


array(Size) ->
    fun
        (<<_:8, JSONB/bytes>> = Input) ->
            ?LOG_DEBUG(#{size => Size, input => Input}),

            (followed_with(
               header(array, Size),
               fun
                   (#{element_count := ElementCount}) ->
                       value_entries(ElementCount, Size, JSONB)
               end))(Input)
    end.


header(Type, Size) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{size => Size, input => Input}),

            (into_map(
               sequence(
                 [kv(type, type(Type, Size)),
                  kv(element_count, element_count(Size)),
                  kv(size, sz(Size))])))(Input)
    end.


value_entries(N, Size, JSONB) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{n => N,
                         jsonb => JSONB,
                         input => Input}),

            (count(N, value_entry(Size, JSONB)))(Input)
    end.


value_entry(Size, JSONB) ->
    fun
        (Input) ->
            (alt(
               [preceded(
                  type(int16),
                  %% in a large array/object an int16 uses 4 bytes
                  msmp_integer_fixed:decode(bytes(Size))),

                condition(
                 Size == large,
                  preceded(
                    type(int32),
                    msmp_integer_fixed:decode(4))),

                literal(Size),

                offset_value({array, small},
                             small,
                             offset_array(small),
                             JSONB),

                offset_value(string,
                             Size,
                             msmp_string_length_encoded:decode(),
                             JSONB)]))(Input)
    end.


offset_array(Size) ->
    fun
        (Input) ->
            (followed_with(
               into_map(
                 sequence(
                   [kv(element_count, element_count(Size)),
                    kv(size, sz(Size))])),
               fun
                   (#{element_count := ElementCount}) ->
                       value_entries(ElementCount, Size, Input)
               end))(Input)
    end.


offset_value(Type, Size, ValueParser, JSONB) ->
    fun
        (Input) ->
            maybe
                {Remaining, #{offset := Offset}} ?= (into_map(
                                                       sequence(
                                                         [kv(type, type(Type)),
                                                          offset(Size)])))(Input),

                ?LOG_DEBUG(#{type => Type,
                             size => Size,
                             value_parser => scran_debug:pp(ValueParser),
                             offset => Offset,
                             input => Input,
                             jsonb => JSONB}),

                {_, Value} ?= (at_offset(ValueParser, Offset))(JSONB),

                ?LOG_DEBUG(#{remaining => Remaining, value => Value}),

                {Remaining, Value}
            end
    end.


at_offset(Parser, Offset) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{parser => scran_debug:pp(Parser),
                         input => Input,
                         offset => Offset}),

            (preceded(
               take(Offset),
               Parser))(Input)
    end.


offset(Size) ->
    fun
        (Input) ->
            (kv(offset, msmp_integer_fixed:decode(bytes(Size))))(Input)
    end.


object() ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{input => Input}),

            (alt([object(small),
                  object(large)]))(Input)
    end.


object(Size) ->
    fun
        (<<_:8, JSONB/bytes>> = Input) ->
            ?LOG_DEBUG(#{size => Size, input => Input}),

            (followed_with(
               header(object, Size),
               fun
                   (#{element_count := ElementCount}) ->
                       object_kv(ElementCount, Size, JSONB)
               end))(Input)
    end.


sz(Size) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{size => Size, input => Input}),
            (msmp_integer_fixed:decode(bytes(Size)))(Input)
    end.


element_count(Size) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{size => Size, input => Input}),
            (msmp_integer_fixed:decode(bytes(Size)))(Input)
    end.


object_kv(N, Size, JSONB) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{n => N,
                         jsonb => JSONB,
                         input => Input}),

            (into_map(
               zip(object_keys(N, Size, JSONB),
                   value_entries(N, Size, JSONB))))(Input)
    end.


object_keys(N, Size, JSONB) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{n => N,
                         size => Size,
                         jsonb => JSONB,
                         input => Input}),

            (count(N, at_offset_with_length(key_entry(Size), JSONB)))(Input)
    end.


key_entry(Size) ->
    fun
        (Input) ->
            (into_tuple(
               sequence(
                 [msmp_integer_fixed:decode(bytes(Size)),
                  %% key length is always 2 bytes independent of size
                  msmp_integer_fixed:decode(2)])))(Input)
    end.


at_offset_with_length(Parser, JSONB) ->
    fun
        (Input) ->
            (map_result(
               Parser,
               fun
                   (PosLen) ->
                       try
                           binary:part(JSONB, PosLen)

                       catch
                           error:badarg ->
                               nomatch
                       end
               end))(Input)
    end.


bytes(single) -> 1;
bytes(small) -> 2;
bytes(large) -> 4.


-spec literal(atom()) -> scran:parser().

literal(Size) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{input => Input}),

            (preceded(
               type(literal),
               alt([literal(null, Size),
                    literal(true, Size),
                    literal(false, Size)])))(Input)
    end.


type(Type, Size) ->
    ?FUNCTION_NAME({Type, Size}).


-spec type(atom() | tuple()) -> scran:parser().

type(Type) ->
    Tag = <<(cache(?FUNCTION_NAME, Type)):8>>,
    fun
        (Input) ->
            (scran_bytes:tag(Tag))(Input)
    end.


-spec literal(atom(), atom()) -> scran:parser().

literal(Type, Size) ->
    Tag = <<(cache(?FUNCTION_NAME, Type)):(bytes(Size))/little-unit:8>>,
    fun
        (Input) ->
            ?LOG_DEBUG(#{type => Type, input => Input}),
            (scran_combinator:value(Type, tag(Tag)))(Input)
    end.


-spec cache(atom(), atom() | tuple()) -> integer() | no_return().

cache(Category, Type) ->
    maps:get(Type, cache(Category)).


-spec cache(atom()) -> #{atom() => integer()} | no_return().

cache(Category) ->
    maps:get(Category, persistent_term:get(?MODULE)).


on_load() ->
    persistent_term:put(
      ?MODULE,
      #{type => msmp_enum:priv_consult("jsonb-type.terms"),
        literal => msmp_enum:priv_consult("jsonb-literal.terms")}).
