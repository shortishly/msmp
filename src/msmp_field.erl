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
-export([decode/3]).
-export([is_character_type/1]).
-export([is_numeric_type/1]).
-export([lookup/1]).
-export([types/0]).
-export_type([type/0]).
-import(scran_bytes, [take/1]).
-import(scran_combinator, [map_result/2]).
-import(scran_combinator, [rest/0]).
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


decode(long, Unsigned, _Metadata) ->
    fun
        (Input) ->
            (fixed(4, Unsigned))(Input)
    end;

decode(longlong = Type, Unsigned, Metadata) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{type => Type,
                         unsigned => Unsigned,
                         metadata => Metadata,
                         input => Input}),
            (fixed(8, Unsigned))(Input)
    end;

decode(tiny, Unsigned, _Metadata) ->
    fun
        (Input) ->
            (fixed(1, Unsigned))(Input)
    end;

decode(short, Unsigned, _Metadata) ->
    fun
        (Input) ->
            (fixed(2, Unsigned))(Input)
    end;

decode(int24, Unsigned, _) ->
    fun
        (Input) ->
            (fixed(3, Unsigned))(Input)
    end;

decode(string, _, #{length := Length, field_type := enum}) ->
    fun
        (Input) ->
            (msmp_integer_fixed:decode(Length))(Input)
    end;

decode(string, _, #{length := Length, field_type := string}) when Length < 256 ->
    fun
        (Input) ->
            (scran_bytes:length_encoded(
                msmp_integer_fixed:decode(1)))(Input)
    end;

decode(string, _, #{field_type := string}) ->
    fun
        (Input) ->
            (scran_bytes:length_encoded(
                msmp_integer_fixed:decode(2)))(Input)
    end;

decode(varchar, _, Length) when is_integer(Length), Length < 256 ->
    fun
        (Input) ->
            (scran_bytes:length_encoded(
                msmp_integer_fixed:decode(1)))(Input)
    end;

decode(varchar, _, Length) when is_integer(Length) ->
    fun
        (Input) ->
            (scran_bytes:length_encoded(
                msmp_integer_fixed:decode(2)))(Input)
    end;

decode(date, Unsigned, _Metadata) ->
    fun
        (Input) ->
            (map_result(
               fixed(3, Unsigned),
               fun
                   (Encoded) ->
                       <<Year:15, Month:4, Date:5>> = <<Encoded:24>>,
                       {Year, Month, Date}
               end))(Input)
    end;

decode(time, Unsigned, _Metadata) ->
    fun
        (Input) ->
            (map_result(
               fixed(3, Unsigned),
               fun
                   (Encoded) ->
                       {Encoded div 10_000,
                        (Encoded div 100) rem 100,
                        Encoded rem 100}
               end))(Input)
    end;

decode(time2, _, 0) ->
    fun
        (Input) ->
            (map_result(
               take(3),
               fun
                   (<<Encoded:24>>) ->
                       case Encoded - 16#80_00_00 of
                           Negative when Negative < 0 ->
                               <<_:2, H:10, M:6, S:6>> = <<(abs(Negative)):24>>,
                               {-H, M, S};

                           Positive ->
                               <<_:2, H:10, M:6, S:6>> = <<Positive:24>>,
                               {H, M, S}
                       end
               end))(Input)
    end;

decode(time2, _, Metadata) when Metadata == 5;
                                Metadata == 6 ->
    fun
        (Input) ->
            (map_result(
               take(6),
               fun
                   (<<Encoded:48>>) ->
                       case Encoded - 16#80_00_00_00_00_00 of
                           Negative when Negative < 0 ->
                               <<_:2, H:10, M:6, S:6, Frac:24>> = <<(abs(Negative)):48>>,
                               {-H, M, S + (Frac / 1_000_000)};

                           Positive ->
                               <<_:2, H:10, M:6, S:6, Frac:24>> = <<Positive:48>>,
                               {H, M, S + (Frac / 1_000_000)}
                       end
               end))(Input)
    end;

decode(time2, _, Metadata) ->
    FracBytes = (Metadata + 1) div 2,
    {Delta, Divisor} = case Metadata of
                           Metadata when Metadata =< 2 ->
                               {16#1_00, 100};

                           Metadata when Metadata =< 4 ->
                               {16#1_00_00, 10_000}
                       end,
    fun
        (Input) ->
            (map_result(
               take(3 + FracBytes),
               fun
                   (<<IntPart:24, FracPart:FracBytes/unit:8>>) ->
                       case IntPart - 16#80_00_00 of
                           Negative when Negative < 0 ->
                               <<_:2, H:10, M:6, S:6>> = <<(abs(Negative)):24>>,
                               {-H, M, S - 1 - ((FracPart - Delta) / Divisor)};

                           Positive ->
                               <<_:2, H:10, M:6, S:6>> = <<Positive:24>>,
                               {H, M, S + (FracPart / Divisor)}
                       end
               end))(Input)
    end;

decode(timestamp2, _, 0) ->
    fun
        (Input) ->
            (map_result(
               take(4),
               fun
                   (<<Second:32>>) ->
                       erlang:convert_time_unit(
                         Second,
                         second,
                         microsecond)
               end))(Input)
    end;

decode(timestamp2, _, Decimals) when Decimals =< 2 ->
    fun
        (Input) ->
            (map_result(
               take(5),
               fun
                   (<<Second:32, Micro:8>>) ->
                       erlang:convert_time_unit(
                         Second,
                         second,
                         microsecond)
                           + (Micro * 10_000)
               end))(Input)
    end;

decode(timestamp2, _, Decimals) when Decimals =< 4 ->
    fun
        (Input) ->
            (map_result(
               take(6),
               fun
                   (<<Second:32, Micro:16>>) ->
                       erlang:convert_time_unit(
                         Second,
                         second,
                         microsecond)
                           + (Micro * 100)
               end))(Input)
    end;

decode(timestamp2, _, Decimals) when Decimals =< 6 ->
    fun
        (Input) ->
            (map_result(
               take(7),
               fun
                   (<<Second:32, Micro:24>>) ->
                       erlang:convert_time_unit(
                         Second,
                         second,
                         microsecond)
                           + Micro
               end))(Input)
    end;

decode(year, _, _) ->
    fun
        (Input) ->
            (map_result(
               take(1),
               fun
                   (<<0:8>>) ->
                       0;

                   (<<Year:8>>) ->
                       Year + 1_900
               end))(Input)
    end;

decode(datetime = Type, Unsigned, Metadata = 0) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{type => Type,
                         unsigned => Unsigned,
                         metadata => Metadata,
                         input => Input}),
            (map_result(
               take(5),
               fun
                   (<<_:1, YM:17, Day:5, Hour:5, Min:6, Sec:6>>) ->
                       {{YM div 13, YM rem 13, Day}, {Hour, Min, Sec}}
               end))(Input)
    end;


decode(datetime, _, abc) ->
    fun
        (Input) ->
            (map_result(
               fixed(8, true),
               fun
                   (Encoded) ->
                       D = Encoded div 1_000_000,
                       T = Encoded rem  1_000_000,
                       {{D div 10_000, (D rem 10_000) div 100, D rem 100},
                        {T div 10_000, (T rem 10_000) div 100, T rem 100}}
               end))(Input)
    end;

decode(datetime2 = Type, Unsigned, Metadata) when Metadata >= 5 ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{type => Type,
                         unsigned => Unsigned,
                         metadata => Metadata,
                         input => Input}),
            (map_result(
               take(8),
               fun
                   (<<_:1, YM:17, Day:5, Hour:5, Min:6, Sec:6, Micro:24>>) ->
                       datetime_to_system_microsecond(
                         {{YM div 13, YM rem 13, Day}, {Hour, Min, Sec}},
                         Micro)
                       %%                       {{YM div 13, YM rem 13, Day}, {Hour, Min, Sec + (Micro / 1_000_000)}}
               end))(Input)
    end;

decode(datetime2 = Type, Unsigned, Metadata) when Metadata >= 3 ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{type => Type,
                         unsigned => Unsigned,
                         metadata => Metadata,
                         input => Input}),
            (map_result(
               take(7),
               fun
                   (<<_:1, YM:17, Day:5, Hour:5, Min:6, Sec:6, Micro:16>>) ->
                       datetime_to_system_microsecond(
                         {{YM div 13, YM rem 13, Day}, {Hour, Min, Sec}},
                           Micro * 100)
                       %% {{YM div 13, YM rem 13, Day}, {Hour, Min, Sec + (Micro * 100 / 1_000_000)}}
               end))(Input)
    end;

decode(datetime2 = Type, Unsigned, Metadata) when Metadata >= 1 ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{type => Type, unsigned => Unsigned, metadata => Metadata, input => Input}),
            (map_result(
               take(6),
               fun
                   (<<_:1, YM:17, Day:5, Hour:5, Min:6, Sec:6, Micro:8>>) ->
                       datetime_to_system_microsecond(
                         {{YM div 13, YM rem 13, Day}, {Hour, Min, Sec}},
                         erlang:convert_time_unit(
                           Micro * 10,
                           millisecond,
                           microsecond))

                       %% datetime_to_system_microsecond({{YM div 13, YM rem 13, Day}, {Hour, Min, Sec}})
                       %%     + erlang:convert_time_unit(
                       %%         Micro * 10,
                       %%         millisecond,
                       %%         microsecond)
                       %% + (Micro * 10_000 / 1_000_000)
               end))(Input)
    end;

decode(datetime2 = Type, Unsigned, 0 = Metadata) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{type => Type, unsigned => Unsigned, metadata => Metadata, input => Input}),
            (map_result(
               take(5),
               fun
                   (<<_:1, YM:17, Day:5, Hour:5, Min:6, Sec:6>>) ->
                       datetime_to_system_microsecond({{YM div 13, YM rem 13, Day}, {Hour, Min, Sec}})
               end))(Input)
    end;

decode(float, _, _) ->
    fun
        (Input) ->
            (map_result(
               scran_number_le:f32(),
               scran_number:precision(6)))(Input)
    end;

decode(double, _, _) ->
    fun
        (Input) ->
            (map_result(
               scran_number_le:f64(),
               scran_number:precision(15)))(Input)
    end;

decode(bit, _, 0) ->
    fun
        (Input) ->
            (fixed(1, false))(Input)
    end;

decode(bit, _, Bits) ->
    fun
        (Input) ->
            (map_result(
               take((Bits + 7) div 8),
               fun
                   (<<_:(8 - (Bits rem 8))/bits, Value:Bits/bits>>) ->
                       Value
               end))(Input)
    end;

decode(_, _, _) ->
    fun
        (Input) ->
            (rest())(Input)
    end.


datetime_to_system_microsecond(DateTime) ->
    ?FUNCTION_NAME(DateTime, 0).

datetime_to_system_microsecond(DateTime, Microsecond) ->
    case erlang:convert_time_unit(
           calendar:datetime_to_gregorian_seconds(DateTime) -
               epoch(posix),
           second,
           microsecond) of
        SystemTime when SystemTime >= 0 ->
            SystemTime + Microsecond;

        SystemTime ->
            SystemTime - Microsecond
    end.


epoch(posix) ->
    calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).


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


fixed(Bytes, true) ->
    scran_number_le:u(Bytes * 8);
fixed(Bytes, false) ->
    scran_number_le:i(Bytes * 8).
