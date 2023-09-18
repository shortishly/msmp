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


-module(msmp_decimal_tests).


-include_lib("eunit/include/eunit.hrl").


packed_byte_size_test_() ->
    lists:map(
      fun
          ({Expected, Precision, Scale} = Test) ->
            {iolist_to_binary(io_lib:fwrite("~p", [Test])),
             ?_assertEqual(
                Expected,
                msmp_decimal:packed_byte_size(Precision, Scale))}
      end,
      [{4 + 4, 18, 9},
       {7 + 3, 20, 6},
       {2, 4, 2},
       {7, 14, 4}]).

%% create table test.t0014 (i serial, q0 decimal(5, 2));
%%
%% insert into test.t0014 (q0) values (-999.99), (0.0), (999.99);
%%
%% <<72,0,0,
%%   205,
%%   0,
%%   225,95,215,100,30,100,0,0,0,71,0,0,0,56,255,51,0,0,0,118,
%%   0,0,0,0,0,1,0,2,0,2,255,
%%
%%   0,
%%   1,0,0,0,0,0,0,0,
%%   124,24,156,         (-999.99)
%%
%%   0,
%%   2,0,0,0,0,0,0,0,
%%   128,0,0,            (0.0)
%%
%%   0,
%%   3,0,0,0,0,0,0,0,
%%   131,231,99,         (999.99)
%%
%%   132,77,248,130>>


%% create table test.t0014 (i serial, q0 decimal(5, 2), q1 decimal(14, 4));
%% insert into test.t0014 (q0) values (-999.99), (0.0), (999.99);
%% insert into test.t0014 (q1) values (1234567890.1234), (0.0), (-1234567890.1234);
%% <<72,0,0,205,0,18,189,215,100,30,100,0,0,0,71,0,0,0,81,255,51,0,0,0,118,
%%   0,0,0,0,0,1,0,2,0,3,255,
%% 4,
%% 1,0,0,0,0,0,0,0,
%% 124,24,156,
%% 4,
%% 2,0,0,0,0,0,0,0,
%% 128,0,0,
%% 4,
%% 3,0,0,0,0,0,0,0,
%% 131,231,99,
%% 72,119,223,133>>

%% <<84,0,0,210,0,18,189,215,100,30,100,0,0,0,83,0,0,0,172,0,52,0,0,0,118,
%%   0,0,0,0,0,1,0,2,0,3,255,
%% 2,
%% 4,0,0,0,0,0,0,0,
%% 129,13,251,56,210,4,210,
%% 2,
%% 5,0,0,0,0,0,0,0,
%% 128,0,0,0,0,0,0,
%% 2,
%% 6,0,0,0,0,0,0,0,
%% 126,242,4,199,45,251,45,
%%
%% 97,0,59,38>>




decode_test_() ->
    lists:map(
      fun
          ({Expected, Input, Precision, Scale} = Test) ->
              {iolist_to_binary(io_lib:fwrite("~p", [Test])),
               ?_assertEqual(
                  Expected,
                  case (msmp_decimal:decode(Precision, Scale))(Input) of
                      {<<>>, Expected} ->
                          Expected;
                      Otherwise ->
                          ?debugVal(Otherwise, -1),
                          Otherwise
                  end)}
      end,
      [{{-999, 99}, <<124, 24, 156>>, 5, 2},
       {{0, 0}, <<128, 0, 0>>, 5, 2},
       {{999, 99}, <<131, 231, 99>>, 5, 2},
       {{ 1234567890, 1234}, <<129,  13, 251,  56, 210,   4, 210>>, 14, 4},
       {{0, 0},              <<128,   0,   0,   0,   0,   0,   0>>, 14, 4},
       {{-1234567890, 1234}, <<126, 242,   4, 199,  45, 251,  45>>, 14, 4},
       {{-10, 55}, <<117, 200>>, 4, 2}]).
