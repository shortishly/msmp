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


-module(msmp_binlog_event_tests).


-import(msmp_tests, [t/1]).
-import(msmp_tests, [t/2]).
-include_lib("eunit/include/eunit.hrl").


decode_test_() ->
    t(msmp_codec:decode(
        msmp_binlog_network_stream:decode(
          #{mapped => #{}})),
      "test/binlog-event.terms").


t3_rows_test_() ->
    lists:map(
      t(msmp_binlog_event:rows(#{flags => 1,
                                  table => <<"t3">>,
                                  metadata => #{unsignedness => #{1 => true,
                                                                  2 => false,
                                                                  3 => true}},
                                  database => <<"test">>,
                                  field_metadata => #{1 => undefined,
                                                      2 => undefined,
                                                      3 => undefined},
                                  table_id => 116,
                                  coltypes => [longlong, tiny, tiny],
                                  null_bitmap => <<6>>})),
      [{[{1, -128, 0}, {2, 127, 255}],
        <<0,               % row 1
          1,0,0,0,0,0,0,0,
          128,0,

          0,               % row 2
          2,0,0,0,0,0,0,0,
          127,255>>}]).
