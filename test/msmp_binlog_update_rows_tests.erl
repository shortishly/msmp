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


-module(msmp_binlog_update_rows_tests).


-import(msmp_tests, [t/1]).
-include_lib("eunit/include/eunit.hrl").

t9_test_() ->
    Mapped = #{122 => #{flags => 1,
                        table => <<"t9">>,
                        metadata => #{unsignedness => #{1 => true,
                                                        2 => false},
                                      column_name => [<<"i">>, <<"p">>],
                                      simple_primary_key => <<0>>,
                                      column_visibility => <<"<C0>">>},
                        database => <<"test">>,
                        field_metadata => #{1 => undefined,
                                            2 => undefined},
                        coltypes => [longlong, tiny],
                        null_bitmap => <<2>>}},

    lists:map(
      t(msmp_codec:decode(
          msmp_binlog_network_stream:decode(
            #{mapped => Mapped}))),
      [{#{packet => #{header => #{flags => 0,
                                  timestamp => 1691499665,
                                  server_id => 100,
                                  event_type => update_rows,
                                  event_size => 56,
                                  log_pos => 3403282},
                      action => log_event,
                      event => #{flags => 1,
                                 columns => 2,
                                 rows => [{{1, -128}, {1, 0}}],
                                 table_id => 122,
                                 extra_row_info => <<>>,
                                 bitmaps => {255, 255}},
                      footer => <<74,225,28,10>>},
          sequence => 207},
        <<57,0,0,          % little endian packet length
          207,
          0,
          145,60,210,100,  % timestamp
          31,              % update rows
          100,0,0,0,
          56,0,0,0,
          18,238,51,0,
          0,0,
          122,0,0,0,0,0,  % table id
          1,0,
          2,0,
          2,              % width of columns
          255,            % BI bitmap
          255,            % AI bitmap

          0,
          1,0,0,0,0,0,0,0,
          128,
          0,
          1,0,0,0,0,0,0,0,
          0,

          74,225,28,10>>},

      {#{packet => #{header => #{flags => 0,
                                 timestamp => 1695301789,
                                 server_id => 100,
                                 event_type => update_rows_compressed_v1,
                                 event_size => 53,
                                 log_pos => 33165},
                     action => log_event,
                     event => #{flags => 1,
                                columns => 2,
                                rows => [{null,null},
                                         {null,20},
                                         {9154974720394001308,96}],
                                table_id => 122,
                                bitmap => 255},
                     footer => <<"î· ¬">>},
         sequence => 163},
       <<54,0,0,
         163,0,157,64,12,101,167,100,0,0,0,53,0,0,0,141,129,0,0,0,0,
         122,0,0,0,0,0,1,0,2,255,255,129,20,120,156,251,195,200,0,1,13,127,96,44,6,
         0,35,56,2,123,238,183,32,172>>}]).
