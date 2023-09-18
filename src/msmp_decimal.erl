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


-module(msmp_decimal).


-export([decode/2]).
-export([packed_byte_size/2]).
-import(scran_bytes, [take/1]).
-import(scran_combinator, [map_result/2]).
-include_lib("kernel/include/logger.hrl").


decode(Precision, Scale) ->
    IntegerBytes = packed_byte_size(Precision - Scale),
    LeftoverBytes = leftover_bytes(Precision - Scale),
    FullBytes = full_bytes(Precision - Scale),

    FractionBytes = packed_byte_size(Scale),

    fun
        (Input) ->

            ?LOG_DEBUG(#{integer_bytes => IntegerBytes,
                         leftover_bytes => LeftoverBytes,
                         full_bytes => FullBytes,
                         input => Input,
                         fraction_bytes => FractionBytes}),

            (map_result(
               take(packed_byte_size(Precision, Scale)),
               fun
                   (<<1:1,
                      Leftover:(LeftoverBytes * 8 - 1),
                      Full:FullBytes/unit:8,
                      Fraction:FractionBytes/unit:8>>) when FullBytes > 0 ->
                       ?LOG_DEBUG(#{left_over => Leftover,
                                    full => Full,
                                    fraction => Fraction}),
                       {list_to_integer(integer_to_list(Leftover) ++ integer_to_list(Full)), Fraction};


                    (<<1:1,
                       Leftover:(LeftoverBytes * 8 - 1),
                       Fraction:FractionBytes/unit:8>>) ->
                       ?LOG_DEBUG(#{leftover => Leftover,
                                    fraction => Fraction}),
                       {Leftover, Fraction};


                   (<<0:1, _/bits>> = Negated) when FullBytes > 0 ->
                       <<_:1,
                         Leftover:(LeftoverBytes * 8 - 1),
                         Full:FullBytes/unit:8,
                         Fraction:FractionBytes/unit:8>> = flip(Negated),

                       ?LOG_DEBUG(#{negated => Negated,
                                    leftover => Leftover,
                                    full => Full,
                                    fraction => Fraction}),
                       {-list_to_integer(integer_to_list(Leftover) ++ integer_to_list(Full)), Fraction};


                   (<<0:1, _/bits>> = Negated) ->
                       <<_:1,
                         Leftover:(LeftoverBytes * 8 - 1),
                         Fraction:FractionBytes/unit:8>> = flip(Negated),

                       ?LOG_DEBUG(#{negated => Negated,
                                    leftover => Leftover,
                                    fraction => Fraction}),
                       {-Leftover, Fraction}
               end))(Input)
    end.


flip(Bytes) ->
    list_to_binary([<<(bnot Byte):8>> || <<Byte:8>> <= Bytes]).


packed_byte_size(Precision, Scale) when Precision > Scale ->
    ?FUNCTION_NAME(Precision - Scale) + ?FUNCTION_NAME(Scale).

packed_byte_size(Digits) ->
    full_bytes(Digits) + leftover_bytes(Digits).


full_bytes(Digits) ->
    (Digits div 9) * 4.


leftover_bytes(Digits) when Digits rem 9 == 0 ->
    0;
leftover_bytes(Digits) when Digits rem 9 =< 2 ->
    1;
leftover_bytes(Digits) when Digits rem 9 =< 4 ->
    2;
leftover_bytes(Digits) when Digits rem 9 =< 6 ->
    3;
leftover_bytes(Digits) when Digits rem 9 =< 8 ->
    4.
