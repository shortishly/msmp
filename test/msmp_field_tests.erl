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


-module(msmp_field_tests).


-import(msmp_tests, [t/1]).
-import(scran_combinator, [map_result/2]).
-include_lib("eunit/include/eunit.hrl").


into_rfc3339(Parser) ->
    fun
        (Input) ->
            (map_result(
               Parser,
               fun
                   (SystemTime) ->
                       calendar:system_time_to_rfc3339(
                         SystemTime,
                         [{unit, microsecond}, {offset, "Z"}])
               end))(Input)
    end.


datetime2_0_test_() ->
    lists:map(
      t(into_rfc3339(msmp_field:decode(datetime2, ignored, 0))),
      [{"1000-01-01T00:00:00.000000Z", <<140, 178,  66,   0,   0>>},
       {"9999-12-31T23:59:59.000000Z", <<254, 243, 255, 126, 251>>}]).

datetime2_1_test_() ->
    lists:map(
      t(into_rfc3339(msmp_field:decode(datetime2, ignored, 1))),
      [{"1000-01-01T00:00:00.600000Z", <<140, 178,  66,   0,   0, 60>>},
       {"9999-12-31T23:59:59.600000Z", <<254, 243, 255, 126, 251, 60>>}]).

datetime2_2_test_() ->
    lists:map(
      t(into_rfc3339(msmp_field:decode(datetime2, ignored, 2))),
      [{"1000-01-01T00:00:00.650000Z", <<140, 178,  66,   0,   0, 65>>},
       {"9999-12-31T23:59:59.650000Z", <<254, 243, 255, 126, 251, 65>>}]).

datetime2_3_test_() ->
    lists:map(
      t(into_rfc3339(msmp_field:decode(datetime2, ignored, 3))),
      [{"1000-01-01T00:00:00.654000Z", <<140, 178,  66,   0,   0, 25, 140>>},
       {"9999-12-31T23:59:59.654000Z", <<254, 243, 255, 126, 251, 25, 140>>}]).

datetime2_4_test_() ->
    lists:map(
      t(into_rfc3339(msmp_field:decode(datetime2, ignored, 4))),
      [{"1000-01-01T00:00:00.654300Z", <<140, 178,  66,   0,   0, 25, 143>>},
       {"9999-12-31T23:59:59.654300Z", <<254, 243, 255, 126, 251, 25, 143>>}]).

datetime2_5_test_() ->
    lists:map(
      t(into_rfc3339(msmp_field:decode(datetime2, ignored, 5))),
      [{"1000-01-01T00:00:00.654320Z", <<140, 178,  66,   0,   0, 9, 251, 240>>},
       {"9999-12-31T23:59:59.654320Z", <<254, 243, 255, 126, 251, 9, 251, 240>>}]).

datetime2_6_test_() ->
    lists:map(
      t(into_rfc3339(msmp_field:decode(datetime2, ignored, 6))),
      [{"1000-01-01T00:00:00.000000Z", <<140, 178,  66,   0,   0,  0,  0,  0>>},
       {"1000-01-01T00:00:00.654321Z", <<140, 178,  66,   0,   0, 9, 251, 241>>},
       {"9999-12-31T23:59:59.654321Z", <<254, 243, 255, 126, 251, 9, 251, 241>>},
       {"9999-12-31T23:59:59.999999Z", <<254, 243, 255, 126, 251, 15, 66, 63>>}]).


timestamp2_0_test_() ->
    lists:map(
      t(into_rfc3339(msmp_field:decode(timestamp2, ignored, 0))),
      [{"1970-01-01T00:00:01.000000Z", <<  0,   0,   0,   1>>},
       {"2038-01-19T03:14:07.000000Z", <<127, 255, 255, 255>>}]).

timestamp2_1_test_() ->
    lists:map(
      t(into_rfc3339(msmp_field:decode(timestamp2, ignored, 1))),
      [{"1970-01-01T00:00:01.600000Z", <<  0,   0,   0,   1, 60>>},
       {"2038-01-19T03:14:07.100000Z", <<127, 255, 255, 255, 10>>}]).

timestamp2_2_test_() ->
    lists:map(
      t(into_rfc3339(msmp_field:decode(timestamp2, ignored, 2))),
      [{"1970-01-01T00:00:01.650000Z", <<  0,   0,   0,   1, 65>>},
       {"2038-01-19T03:14:07.120000Z", <<127, 255, 255, 255, 12>>}]).

timestamp2_3_test_() ->
    lists:map(
      t(into_rfc3339(msmp_field:decode(timestamp2, ignored, 3))),
      [{"1970-01-01T00:00:01.654000Z", <<  0,   0,   0,   1, 25, 140>>},
       {"2038-01-19T03:14:07.123000Z", <<127, 255, 255, 255,  4, 206>>}]).

timestamp2_4_test_() ->
    lists:map(
      t(into_rfc3339(msmp_field:decode(timestamp2, ignored, 4))),
      [{"1970-01-01T00:00:01.654300Z", <<  0,   0,   0,   1, 25, 143>>},
       {"2038-01-19T03:14:07.123400Z", <<127, 255, 255, 255,  4, 210>>}]).

timestamp2_5_test_() ->
    lists:map(
      t(into_rfc3339(msmp_field:decode(timestamp2, ignored, 5))),
      [{"1970-01-01T00:00:01.654320Z", <<  0,   0,   0,   1, 9, 251, 240>>},
       {"2038-01-19T03:14:07.123450Z", <<127, 255, 255, 255, 1, 226,  58>>}]).

timestamp2_6_test_() ->
    lists:map(
      t(into_rfc3339(msmp_field:decode(timestamp2, ignored, 6))),
      [{"1970-01-01T00:00:01.654321Z", <<  0,   0,   0,   1, 9, 251, 241>>},
       {"2038-01-19T03:14:07.499999Z", <<127, 255, 255, 255, 7, 161,  31>>}]).


time2_0_test_() ->
    lists:map(
      t(msmp_field:decode(time2, ignored, 0)),
      [{{-838, 59, 59}, << 75, 145,   5>>},
       {{ 838, 59, 59}, <<180, 110, 251>>}]).

time2_1_test_() ->
    lists:map(
      t(msmp_field:decode(time2, ignored, 1)),
      [{{-838, 59, 58.6}, << 75, 145,   5, 196>>},
       {{ 838, 59, 58.1}, <<180, 110, 250,  10>>}]).

time2_2_test_() ->
    lists:map(
      t(msmp_field:decode(time2, ignored, 2)),
      [{{-838, 59, 58.65}, << 75, 145,   5, 191>>},
       {{ 838, 59, 58.12}, <<180, 110, 250,  12>>}]).

time2_3_test_() ->
    lists:map(
      t(msmp_field:decode(time2, ignored, 3)),
      [{{-838, 59, 58.654}, << 75, 145,   5, 230, 116>>},
       {{ 838, 59, 58.123}, <<180, 110, 250,   4, 206>>}]).

time2_4_test_() ->
    lists:map(
      t(msmp_field:decode(time2, ignored, 4)),
      [{{-838, 59, 58.6543}, << 75, 145,   5, 230, 113>>},
       {{ 838, 59, 58.1234}, <<180, 110, 250,   4, 210>>}]).

time2_5_test_() ->
    lists:map(
      t(msmp_field:decode(time2, ignored, 5)),
      [{{-838, 59, 58.65432}, << 75, 145,   5, 246,   4, 16>>},
       {{ 838, 59, 58.12345}, <<180, 110, 250,   1, 226, 58>>}]).

time2_6_test_() ->
    lists:map(
      t(msmp_field:decode(time2, ignored, 6)),
      [{{-838, 59, 59.00000}, << 75, 145,   5, 0, 0, 0>>},
       {{ 838, 59, 59.00000}, <<180, 110, 251, 0, 0, 0>>}]).


year_test_() ->
    lists:map(
      t(msmp_field:decode(year, ignored, ignored)),
      [{1901, <<  1>>},
       {2155, <<255>>},
       {0000, <<  0>>}]).

varchar_1_byte_length_test_() ->
    lists:map(
      t(msmp_field:decode(varchar, ignored, 255)),
      [{<<"a">>, <<1, 97>>},
       {<<"abcde">>, <<5, "abcde">>},
       {iolist_to_binary(lists:duplicate(51, "abcde")),
        iolist_to_binary([255, lists:duplicate(51, "abcde")])}]).

varchar_2_byte_length_test_() ->
    lists:map(
      t(msmp_field:decode(varchar, ignored, 8_192)),
      [{<<"a">>, <<1, 0, 97>>},
       {<<"abcde">>, <<5, 0, "abcde">>},
       {iolist_to_binary(lists:duplicate(51, "abcde")),
        iolist_to_binary([<<(51*5):16/little>>, lists:duplicate(51, "abcde")])}]).


%% t(Parser) ->
%%     fun
%%         ({Expected, Input} = Test) ->
%%             {iolist_to_binary(io_lib:fwrite("~p", [Test])),
%%              ?_assertEqual(
%%                 Expected,
%%                 case (map_result(
%%                         Parser,
%%                         fun
%%                             (SystemTime) ->
%%                                 calendar:system_time_to_rfc3339(
%%                                   SystemTime,
%%                                   [{unit, microsecond}, {offset, "Z"}])
%%                         end))(Input) of

%%                     {<<>>, Expected} ->
%%                         Expected;

%%                     Otherwise ->
%%                         ?debugVal(Input, -1),
%%                         ?debugVal(Otherwise, -1),
%%                         Otherwise
%%                 end)}
%%     end.


%% <<75,0,0,
%% 221,0,18,189,215,100,30,100,0,0,0,74,0,0,0,103,4,52,0,0,0,119,
%%   0,0,0,0,0,1,0,2,0,2,255,
%% 0,
%% 1,0,0,0,0,0,0,0,
%% 92,255,121,196,
%% 0,
%% 2,0,0,0,0,0,0,0,
%% 0,0,0,0,
%% 0,
%% 3,0,0,0,0,0,0,0,
%% 92,255,121,68,
%%
%% 211,123,13,114>>
float_test_() ->
    lists:map(
      t(msmp_field:decode(float, ignored, ignored)),
      [{-999.99, << 92, 255, 121, 196>>},
       {   0.00, <<  0,   0,   0,   0>>},
       { 999.99, << 92, 255, 121,  68>>}]).


%% <<87,0,0,
%% 232,0,18,189,215,100,30,100,0,0,0,86,0,0,0,57,8,52,0,0,0,120,0,
%%   0,0,0,0,1,0,2,0,2,255,
%% 0,
%% 1,0,0,0,0,0,0,0,
%% 82,184,30,133,235,63,143,192,
%%
%% 0,
%% 2,0,0,0,0,0,0,0,
%% 0,0,0,0,0,0,0,0,
%%
%% 0,
%% 3,0,0,0,0,0,0,0,
%% 82,184,30,133,235,63,143,64,
%%
%% 253,19,188,230>>
double_test_() ->
    lists:map(
      t(msmp_field:decode(double, ignored, ignored)),
      [{-999.99, << 82, 184,  30, 133, 235,  63, 143, 192>>},
       {   0.00, <<  0,   0,   0,   0,   0,   0,   0,   0>>},
       { 999.99, << 82, 184,  30, 133, 235,  63, 143,  64>>}]).

%% <<76,0,0,
%% 243,0,18,189,215,100,30,100,0,0,0,75,0,0,0,247,11,52,0,0,0,121,
%%   0,0,0,0,0,1,0,2,0,2,255,
%% 0,
%% 1,0,0,0,0,0,0,0,
%% 5,
%% 0,
%% 2,0,0,0,0,0,0,0,
%% 0,
%% 0,
%% 3,0,0,0,0,0,0,0,
%% 7,
%% 0,
%% 4,0,0,0,0,0,0,0,
%% 63,
%% 38,177,221,205>>
bit_test_() ->
    lists:map(
      t(msmp_field:decode(bit, ignored, 6)),
      [{<< 5:6>>, <<  5>>},
       {<< 0:6>>, <<  0>>},
       {<< 7:6>>, <<  7>>},
       {<<63:6>>, << 63>>}]).


%%create table test.t0018 (i serial, q0 json);
%% insert into test.t0018 (q0) values ('["abc", 10, null, true, false]'), ('{"k1": "value", "k2": 10}'), ('["12:18:29.000000", "2015-07-29", "2015-07-29 12:18:29.000000"]');
%% meta => 4.
%%
%% <<196,0,0,
%% 254,0,18,189,215,100,30,100,0,0,0,195,0,0,0,42,16,52,0,0,0,
%%   122,0,0,0,0,0,1,0,2,0,2,255,
%% 0,
%% 1,0,0,0,0,0,0,0,
%% 24,0,0,0, - length 24
%% 2,        - small json array
%% 5,0,      - item count 5
%% 23,0,     - size
%% 12,       - type string
%% 19,0,     - offset 19
%% 5,        - int16 (inline)
%% 10,0,     - value (10)
%% 4,        - literal (true/false/null)
%% 0,0       - value (null)
%% 4,        - literal (true/false/null)
%% 1,0,      - value (true)
%% 4,        - literal (true/false/null)
%% 2,0,      - value (false)
%% 3,
%% 97,98,99,
%%
%% 0,
%% 2,0,0,0,0,0,0,0,
%% 29,0,0,0,
%% 0,2,0,28,0,18,0,2,0,20,0,2,0,12,22,0,5,10,0,107,49,107,50,5,118,97,108,
%%   117,101,
%%
%% 0,
%% 3,0,0,0,0,0,0,0,
%% 68,0,0,0,
%% 2,3,0,67,0,12,13,0,12,29,0,12,40,0,
%%   15,49,50,58,49,56,58,50,57,46,48,48,48,48,48,48,10,50,48,49,53,45,48,
%%   55,45,50,57,26,50,48,49,53,45,48,55,45,50,57,32,49,50,58,49,56,58,50,
%%   57,46,48,48,48,48,48,48,147,165,201,109>>

%% (b'101'), (b'0'), (b'111'), (b'111111');

%% newdecimal_test_() ->
%%     lists:map(
%%       fun
%%           ({Input, Precision, Scale, Expected} = Test) ->
%%             {iolist_to_binary(io_lib:fwrite("~p", [Test])),
%%              ?_assertEqual(
%%                 Expected,
%%                 case (msmp_field:decode(
%%                         newdecimal,
%%                         ignored,
%%                         #{scale => Scale, precision => Precision}))(Input) of

%%                     {<<>>, Expected} ->
%%                         Expected;

%%                     Otherwise ->
%%                         ?debugVal(Input, -1),
%%                         ?debugVal(Otherwise, -1),
%%                         Otherwise
%%                 end)}
%%       end,
%%       [{<<117, 200, 127, 255>>, 4, 2, "-10.55"}]).
