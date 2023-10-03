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


-module(msmp_binlog_write_rows_tests).


-import(msmp_tests, [t/1]).
-import(msmp_tests, [t/2]).
-include_lib("eunit/include/eunit.hrl").


tiny_test_() ->
    Mapped = #{116 => #{flags => 1,
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
                        null_bitmap => <<6>>}},

    lists:map(
      t(msmp_codec:decode(
          msmp_binlog_network_stream:decode(
            #{mapped => Mapped}))),

      [{#{packet => #{header => #{flags => 0,
                                  timestamp => 1691145085,
                                  server_id => 100,
                                  event_type => write_rows,
                                  event_size => 57,
                                  log_pos => 3042913},
                      action => log_event,
                      event => #{flags => 1,
                                 rows => [{1, -128, 0}, {2, 127, 255}],
                                 columns => 3,
                                 bitmap => 255,
                                 table_id => 116,
                                 extra_row_info => <<>>},
                      footer => <<50,149,254,21>>},
          sequence => 81},

        <<58,0,0,          % little endian packet length
          81,              % sequence
          0,               % ok packet
          125,211,204,100, % timestamp
          30,              % write rows
          100,0,0,0,       % server id
          57,0,0,0,        % event size
          97,110,46,0,     % log position
          0,0,             % flags
          116,0,0,0,0,0,   % table id
          1,0,             % flags
          2,0,             % extra row info size inclusive (2 - 2)
          3,               % width of columns bitmap
          255,

          0,
          1,0,0,0,0,0,0,0, % row 1
          128,0,

          0,
          2,0,0,0,0,0,0,0, % row 2
          127,255,

          50,149,254,21>>}]).


short_test_() ->
    Mapped = #{117 => #{flags => 1,
                        table => <<"t4">>,
                        metadata => #{unsignedness => #{1 => true,
                                                        2 => false,
                                                        3 => true}},
                        database => <<"test">>,
                        field_metadata => #{1 => undefined,
                                            2 => undefined,
                                            3 => undefined},
                        coltypes => [longlong,short,short],
                        null_bitmap => <<6>>}},

    lists:map(
      t(msmp_codec:decode(
          msmp_binlog_network_stream:decode(
            #{mapped => Mapped}))),

      [{#{packet => #{header => #{flags => 0,
                                  timestamp => 1691145085,
                                  server_id => 100,
                                  event_type => write_rows,
                                  event_size => 90,
                                  log_pos => 3043885},
                      action => log_event,
                      event => #{flags => 1,
                                 columns => 3,
                                 rows => [{1, -32768, null},
                                          {2, -1, null},
                                          {3, 0, null},
                                          {4, 1, null},
                                          {5, 32767, null}],
                                 bitmap => 255,
                                 table_id => 117,
                                 extra_row_info => <<>>},
                      footer => <<"`½R³">>},
          sequence => 92},

           <<91,0,0,          % little endian packet length
             92,              % sequence
             0,               % ok packet
             125,211,204,100, % timestamp
             30,              % write rows
             100,0,0,0,       % server id
             90,0,0,0,        % event size
             45,114,46,0,     % log position
             0,0,             % flags
             117,0,0,0,0,0,   % table id
             1,0,             % flags
             2,0,             % extra row info size inclusive (2 - 2)
             3,               % width of columns bitmap
             255,             % bitmap

             4,               % row 1
             1,0,0,0,0,0,0,0,
             0,128,

             4,               % row 2
             2,0,0,0,0,0,0,0,
             255,255,

             4,               % row 3
             3,0,0,0,0,0,0,0,
             0,0,

             4,               % row 4
             4,0,0,0,0,0,0,0,
             1,0,

             4,               % row 5
             5,0,0,0,0,0,0,0,
             255,127,

             96,189,82,179>>},

           {#{packet => #{header => #{flags => 0,
                                      timestamp => 1691145085,
                                      server_id => 100,
                                      event_type => write_rows,
                                      event_size => 57,
                                      log_pos => 3044183},
                          action => log_event,
                          event => #{flags => 1,
                                     columns => 3,
                                     rows => [{6, null, 0},
                                              {7, null, 65535}],
                                     bitmap => 255,
                                     table_id => 117,
                                     extra_row_info => <<>>},
                          footer => <<135,252,247,69>>},
              sequence => 97},
            <<58,0,0,          % little endian packet length
              97,              % sequence
              0,               % ok packet
              125,211,204,100, % timestamp
              30,              % write rows
              100,0,0,0,       % server id
              57,0,0,0,        % event size
              87,115,46,0,     % log position
              0,0,
              117,0,0,0,0,0,   % table id
              1,0,
              2,0,
              3,               % width of columns bitmap
              255,             % bitmap

              2,               % row 6
              6,0,0,0,0,0,0,0,
              0,0,

              2,               % row 7
              7,0,0,0,0,0,0,0,
              255,255,

              135,252,247,69>>}]).


int24_test_() ->
    Mapped = #{118 => #{flags => 1,
                        table => <<"t5">>,
                        metadata => #{unsignedness => #{1 => true,
                                                        2 => false,
                                                        3 => true},
                                      column_name => [<<"i">>,<<"p">>,<<"q">>],
                                      simple_primary_key => <<0>>,
                                      column_visibility => <<"<E0>">>},
                        table_id => 118,
                        database => <<"test">>,
                        field_metadata => #{1 => undefined,
                                            2 => undefined,
                                            3 => undefined},
                        coltypes => [longlong, int24, int24],
                        null_bitmap => <<6>>}},
    lists:map(
      t(msmp_codec:decode(
          msmp_binlog_network_stream:decode(
            #{mapped => Mapped}))),
      [{#{packet => #{header => #{flags => 0,
                                  timestamp => 1691415393,
                                  server_id => 100,
                                  event_type => write_rows,
                                  event_size => 95,
                                  log_pos => 3395517},
                      action => log_event,
                      event => #{flags => 1,
                                 columns => 3,
                                 rows => [{1, -8388608, null},
                                          {2,       -1, null},
                                          {3,        0, null},
                                          {4,        1, null},
                                          {5,  8388607, null}],
                                 bitmap => 255,
                                 table_id => 118,
                                 extra_row_info => <<>>},
                      footer => <<205,156,17,150>>},
          sequence => 108},
        <<96,0,0,108,0,97,243,208,100,30,100,0,0,0,95,0,0,0,189,207,51,0,0,0,
          118,0,0,0,0,0,1,0,2,0,3,255,4,1,0,0,0,0,0,0,0,0,0,128,4,2,0,0,0,0,0,0,
          0,255,255,255,4,3,0,0,0,0,0,0,0,0,0,0,4,4,0,0,0,0,0,0,0,1,0,0,4,5,0,0,
          0,0,0,0,0,255,255,127,205,156,17,150>>},

      {#{packet => #{header => #{flags => 0,
                                 timestamp => 1691415393,
                                 server_id => 100,
                                 event_type => write_rows,
                                 event_size => 59,
                                 log_pos => 3395831},
                     action => log_event,
                     event => #{flags => 1,
                                columns => 3,
                                rows => [{6, null,        0},
                                         {7, null, 16777215}],
                                bitmap => 255,
                                table_id => 118,
                                extra_row_info => <<>>},
                     footer => <<"@¤¿Ó">>},
         sequence => 113},
       <<60,0,0,113,0,97,243,208,100,30,100,0,0,0,59,0,0,0,247,208,51,0,0,0,
         118,0,0,0,0,0,1,0,2,0,3,255,2,6,0,0,0,0,0,0,0,0,0,0,2,7,0,0,0,0,0,0,0,
         255,255,255,64,164,191,211>>}]).

t1_test_() ->
    Mapped = #{34 => #{flags => 1,
                       table => <<"t1">>,
                       metadata => #{unsignedness => #{1 => false},
                                     column_name => [<<"i">>,
                                                     <<"msg">>,
                                                     <<"d">>,
                                                     <<"t">>,
                                                     <<"dt">>],
                                     default_charset => "-",
                                     simple_primary_key => [0]},
                       database => <<"test">>,
                       field_metadata => #{1 => undefined,
                                           2 => 200,
                                           3 => undefined,
                                           4 => 0,
                                           5 => 0},
                       coltypes => [long,
                                    varchar,
                                    date,
                                    time2,
                                    datetime2],
                       null_bitmap => <<30>>}},
    lists:map(
      t(msmp_codec:decode(
          msmp_binlog_network_stream:decode(
            #{mapped => Mapped}))),
      [{#{packet => #{header => #{flags => 0,
                                  timestamp => 1695222974,
                                  event_type => write_rows_v1,
                                  server_id => 100,
                                  event_size => 55,
                                  log_pos => 93277},
                      action => log_event,
                      event => #{flags => 1,
                                 columns => 5,
                                 rows => [{1, <<"test1">>, {2023,9,20}, {15,16,25}, 1695222974000000}],
                                 table_id => 34,
                                 bitmap => 255},
                      footer => <<217,59,128,77>>},
          sequence => 33},
        <<56,0,0,33,0,190,12,11,101,23,100,0,0,0,55,0,0,0,93,108,1,0,0,0,34,0,0,
          0,0,0,1,0,5,255,224,1,0,0,0,5,116,101,115,116,49,52,207,15,128,244,25,
          153,177,40,244,14,217,59,128,77>>}]).


t6_test_() ->
    Mapped = #{119 => #{flags => 1,
                        table => <<"t6">>,
                        metadata => #{unsignedness => #{1 => true,
                                                        2 => false,
                                                        3 => true},
                                      column_name => [<<"i">>, <<"p">>, <<"q">>],
                                      simple_primary_key => <<0>>,
                                      column_visibility => <<"<E0>">>},
                        database => <<"test">>,
                        field_metadata => #{1 => undefined,
                                            2 => undefined,
                                            3 => undefined},
                        coltypes => [longlong, long, long],
                        null_bitmap => <<6>>}},
    lists:map(
      t(msmp_codec:decode(
          msmp_binlog_network_stream:decode(
            #{mapped => Mapped}))),
      [{#{packet => #{header => #{flags => 0,
                                  timestamp => 1691415393,
                                  server_id => 100,
                                  event_type => write_rows,
                                  event_size => 100,
                                  log_pos => 3396817},
                      action => log_event,
                      event => #{flags => 1,
                                 columns => 3,
                                 rows => [{1, -2147483648, null},
                                          {2, -1, null},
                                          {3, 0, null},
                                          {4, 1, null},
                                          {5, 2147483647, null}],
                                 bitmap => 255,
                                 table_id => 119,
                                 extra_row_info => <<>>},
                      footer => <<65,252,114,140>>},
          sequence => 124},
        <<101,0,0,124,0,97,243,208,100,30,100,0,0,0,100,0,0,0,209,212,51,0,0,0,
          119,0,0,0,0,0,1,0,2,0,3,255,4,1,0,0,0,0,0,0,0,0,0,0,128,4,2,0,0,0,0,0,
          0,0,255,255,255,255,4,3,0,0,0,0,0,0,0,0,0,0,0,4,4,0,0,0,0,0,0,0,1,0,0,
          0,4,5,0,0,0,0,0,0,0,255,255,255,127,65,252,114,140>>},

      {#{packet => #{header => #{flags => 0,
                                 timestamp => 1691415393,
                                 server_id => 100,
                                 event_type => write_rows,
                                 event_size => 61,
                                 log_pos => 3397133},
                     action => log_event,
                     event => #{flags => 1,
                                columns => 3,
                                rows => [{6, null, 0},
                                         {7, null, 4294967295}],
                                bitmap => 255,
                                table_id => 119,
                                extra_row_info => <<>>},
                     footer => <<37,100,4,197>>},
         sequence => 129},
       <<62,0,0,129,0,97,243,208,100,30,100,0,0,0,61,0,0,0,13,214,51,0,0,0,119,
         0,0,0,0,0,1,0,2,0,3,255,2,6,0,0,0,0,0,0,0,0,0,0,0,2,7,0,0,0,0,0,0,0,
         255,255,255,255,37,100,4,197>>}]).


t7_test_() ->
    Mapped = #{120 => #{flags => 1,table => <<"t7">>,
                        metadata => #{unsignedness => #{1 => true,
                                                        2 => false,
                                                        3 => true},
                                      column_name => [<<"i">>, <<"p">>, <<"q">>],
                                      simple_primary_key => <<0>>,
                                      column_visibility => <<"<E0>">>},
                        database => <<"test">>,
                        field_metadata => #{1 => undefined,
                                            2 => undefined,
                                            3 => undefined},
                        coltypes => [longlong, longlong, longlong],
                        null_bitmap => <<6>>}},
        lists:map(
          t(msmp_codec:decode(
              msmp_binlog_network_stream:decode(
                #{mapped => Mapped}))),
          [{#{packet => #{header => #{flags => 0,
                                      timestamp => 1691415393,
                                      server_id => 100,
                                      event_type => write_rows,
                                      event_size => 120,
                                      log_pos => 3398145},
                          action => log_event,
                          event => #{flags => 1,
                                     columns => 3,
                                     rows => [{1, -9223372036854775808, null},
                                              {2,                   -1, null},
                                              {3,                    0, null},
                                              {4,                    1, null},
                                              {5,  9223372036854775807, null}],
                                     bitmap => 255,
                                     table_id => 120,
                                     extra_row_info => <<>>},
                          footer => <<200,154,91,2>>},
              sequence => 140},
            <<121,0,0,140,0,97,243,208,100,30,100,0,0,0,120,0,0,0,1,218,51,0,0,0,
              120,0,0,0,0,0,1,0,2,0,3,255,4,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,4,2,0,
              0,0,0,0,0,0,255,255,255,255,255,255,255,255,4,3,0,0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,4,4,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,4,5,0,0,0,0,0,0,0,255,255,
              255,255,255,255,255,127,200,154,91,2>>},

          {#{packet => #{header => #{flags => 0,
                                     timestamp => 1691415393,
                                     server_id => 100,
                                     event_type => write_rows,
                                     event_size => 69,
                                     log_pos => 3398469},
                         action => log_event,
                         event => #{flags => 1,
                                    columns => 3,
                                    rows => [{6, null,                    0},
                                             {7, null, 18446744073709551615}],
                                    bitmap => 255,
                                    table_id => 120,
                                    extra_row_info => <<>>},
                         footer => <<26,3,192,17>>},
             sequence => 145},
           <<70,0,0,145,0,97,243,208,100,30,100,0,0,0,69,0,0,0,69,219,51,0,0,0,120,
             0,0,0,0,0,1,0,2,0,3,255,2,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,7,0,0,0,0,
             0,0,0,255,255,255,255,255,255,255,255,26,3,192,17>>}]).

t8_test_() ->
    Mapped = #{121 => #{flags => 1,
                        table => <<"t8">>,
                        metadata => #{unsignedness => #{1 => true,
                                                        9 => true},
                                      column_name => [<<"i">>,
                                                      <<"p">>,
                                                      <<"q0">>,
                                                      <<"q6">>,
                                                      <<"r0">>,
                                                      <<"r6">>,
                                                      <<"s0">>,
                                                      <<"s6">>,
                                                      <<"t">>],
                                      simple_primary_key => <<0>>,
                                      column_visibility => <<255,128>>},
                        database => <<"test">>,
                        field_metadata => #{1 => undefined,
                                            2 => undefined,
                                            3 => 0,
                                            4 => 6,
                                            5 => 0,
                                            6 => 6,
                                            7 => 0,
                                            8 => 6,
                                            9 => undefined},
              coltypes => [longlong,
                           date,
                           datetime2,
                           datetime2,
                           timestamp2,
                           timestamp2,
                           time2,
                           time2,
                           year],
              null_bitmap => <<254,1>>}},
        lists:map(
          t(msmp_codec:decode(
              msmp_binlog_network_stream:decode(
                #{mapped => Mapped}))),
          [{#{packet => #{header => #{flags => 0,
                                      timestamp => 1691499665,
                                      server_id => 100,
                                      event_type => write_rows,
                                      event_size => 62,
                                      log_pos => 3399527},
                          action => log_event,
                          event => #{flags => 1,
                                     columns => 9,
                                     rows => [{1, {1000,  1,  1}, null, null, null, null, null, null, null},
                                              {2, {9999, 12, 31}, null, null, null, null, null, null, null}],
                           bitmap => 65_535,table_id => 121,
                           extra_row_info => <<>>},
                     footer => <<0,232,8,167>>},
               sequence => 156},
            <<63,0,0,156,0,145,60,210,100,30,100,0,0,0,62,0,0,0,103,223,51,0,0,0,
              121,0,0,0,0,0,1,0,2,0,9,255,255,252,1,1,0,0,0,0,0,0,0,33,208,7,252,1,
              2,0,0,0,0,0,0,0,159,31,78,0,232,8,167>>},

          {#{packet => #{header => #{flags => 0,
                                     timestamp => 1691499665,
                                     server_id => 100,
                                     event_type => write_rows,
                                     event_size => 66,
                                     log_pos => 3399888},
                         action => log_event,
                         event => #{flags => 1,
                                    columns => 9,
                                    rows => [{3, null, -30610224000000000, null, null, null, null, null, null},
                                             {4, null, 253402300799000000, null, null, null, null, null, null}],
                                    bitmap => 65535,
                                    table_id => 121,
                                    extra_row_info => <<>>},
                         footer => <<"`òÁn">>},
             sequence => 161},
           <<67,0,0,                % little endian packet length
             161,                   % seq
             0,                     % ok
             145,60,210,100,        % timestamp
             30,                    % write rows
             100,0,0,0,             % server id
             66,0,0,0,              % event size
             208,224,51,0,          % log position
             0,0,                   % flags
             121,0,0,0,0,0,         % table id
             1,0,                   % flags
             2,0,                   % extra row info (2 - 2)
             9,                     % column count
             255,255,               % bitmap

             250,1,                 % row null bitmap
             3,0,0,0,0,0,0,0,       % i
             140,178,66,0,0,        % q0 - '1000-01-01 00:00:00'


             250,1,                 % row null bitmap
             4,0,0,0,0,0,0,0,       % i
             254,243,255,126,251,   % q0 - '9999-12-31 23:59:59'

             96,242,193,110>>},

           {#{packet => #{header => #{flags => 0,
                                      timestamp => 1691499665,
                                      server_id => 100,
                                      event_type => write_rows,
                                      event_size => 64,
                                      log_pos => 3400614},
                          action => log_event,
                          event => #{flags => 1,
                                     columns => 9,
                                     rows => [{7, null, null, null, 1000000, null, null, null, null},
                                              {8, null, null, null, 2147483647000000, null, null, null, null}],
                                     bitmap => 65535,
                                     table_id => 121,
                                     extra_row_info => <<>>},
                          footer => <<"h¥1W">>},
              sequence => 171},
            <<65,0,0,
              171,
              0,
              145,60,210,100,
              30,
              100,0,0,0,
              64,0,0,0,
              166,227,51,0,
              0,0,
              121,0,0,0,0,0,
              1,0,
              2,0,
              9,255,255,

              238,
              1,
              7,0,0,0,0,0,0,0,
              0,0,0,1,

              238,
              1,
              8,0,0,0,0,0,0,0,
              127,255,255,255,

              104,165,49,87>>},

          {#{packet => #{header => #{flags => 0,
                                     timestamp => 1691499665,
                                     server_id => 100,
                                     event_type => write_rows,
                                     event_size => 72,
                                     log_pos => 3400255},
                         action => log_event,
                         event => #{flags => 1,
                                    columns => 9,
                                    rows => [{5, null, null, -30610224000000000, null, null, null, null, null},
                                             {6, null, null, 253402300799999999, null, null, null, null, null}],
                                    bitmap => 65535,
                                    table_id => 121,
                                    extra_row_info => <<>>},
                         footer => <<194,81,141,133>>},
             sequence => 166},
           <<73,0,0,
             166,
             0,
             145,60,210,100,
             30,
             100,0,0,0,
             72,0,0,0,
             63,226,51,0,
             0,0,
             121,0,0,0,0,0,         % table id
             1,0,
             2,0,
             9,                     % column count
             255,255,

             246,1,                         % row null bitmap
             5,0,0,0,0,0,0,0,               % i
             140,178,66,0,0,0,0,0,          % q0 - '1000-01-01 00:00:00.000000'

             246,1,                         % row null bitmap
             6,0,0,0,0,0,0,0,               % i
             254,243,255,126,251,15,66,63,  % q0 - '9999-12-31 23:59:59.999999'

             194,81,141,133>>}]).


time_zone_name_test_() ->
    Mapped = #{105 => #{flags => 1,
                        table => <<"time_zone_name">>,
                        metadata => #{unsignedness => #{2 => true},
                                      default_charset => <<"!">>,
                                      column_name => [<<"Name">>, <<"Time_zone_id">>],
                                      simple_primary_key => <<0>>,
                                      column_visibility => <<"À">>},
                        field_metadata => #{1 => #{length => 192, field_type => string},
                                            2 => undefined},
                        coltypes => [string,long],
                        database => <<"mysql">>,
                        null_bitmap => <<0>>}},
    lists:map(
      t(msmp_codec:decode(
          msmp_binlog_network_stream:decode(
            #{mapped => Mapped}))),
      [{#{packet => #{header => #{flags => 0,
                                  timestamp => 1691335990,
                                  server_id => 100,
                                  event_type => write_rows,
                                      event_size => 64,
                                  log_pos => 2703753},
                      action => log_event,
                      event => #{flags => 1,
                                 columns => 2,
                                 rows => [{<<"right/Antarctica/Vostok">>,1427}],
                                 bitmap => 255,
                                 table_id => 105,
                                 extra_row_info => <<>>},
                      footer => <<"¢ã]ë">>},
          sequence => 75},
        <<65,0,0,75,0,54,189,207,100,30,100,0,0,0,64,0,0,0,137,65,41,0,0,0,
          105,0,0,0,0,0,1,0,2,0,2,255,0,23,114,105,103,104,116,47,65,110,
          116,97,114,99,116,105,99,97,47,86,111,115,116,111,107,147,5,0,0,
          162,227,93,235>>}]).

write_rows_v1_test_() ->
    Mapped = #{25 => #{flags => 1,
                       table => <<"t0010">>,
                       metadata => #{unsignedness => #{1 => true},
                                     column_name =>
                                         [<<"i">>,<<"q0">>,<<"q1">>,<<"q2">>,<<"q3">>,<<"q4">>,
                                          <<"q5">>,<<"q6">>],
                                     simple_primary_key => [0]},
                       database => <<"test">>,
                       field_metadata => #{1 => undefined,
                                           2 => 0,
                                           3 => 1,
                                           4 => 2,
                                           5 => 3,
                                           6 => 4,
                                           7 => 5,
                                           8 => 6},
                       coltypes => [longlong,datetime2,datetime2,datetime2,datetime2,datetime2,
                                    datetime2,datetime2],
                       null_bitmap => <<"<FE>">>},

               26 => #{flags => 1,
                       table => <<"t0011">>,
                       metadata => #{unsignedness => #{1 => true},
                                     column_name =>
                                         [<<"i">>,
                                          <<"q0">>,
                                          <<"q1">>,
                                          <<"q2">>,
                                          <<"q3">>,
                                          <<"q4">>,
                                          <<"q5">>,
                                          <<"q6">>],
                                     simple_primary_key => [0]},
                       database => <<"test">>,
                       field_metadata => #{1 => undefined,
                                           2 => 0,
                                           3 => 1,
                                           4 => 2,
                                           5 => 3,
                                           6 => 4,
                                           7 => 5,
                                           8 => 6},
                       coltypes => [longlong,
                                    timestamp2,
                                    timestamp2,
                                    timestamp2,
                                    timestamp2,
                                    timestamp2,
                                    timestamp2,
                                    timestamp2],
                       null_bitmap => <<"<FE>">>}},
    lists:map(
      t(msmp_codec:decode(
          msmp_binlog_network_stream:decode(
            #{mapped => Mapped}))),
      [{#{packet => #{header => #{flags => 0,
                                  timestamp => 1695222973,
                                  event_type => write_rows_v1,
                                  server_id => 100,
                                  event_size => 61,
                                  log_pos => 1362},
                      action => log_event,
                      event => #{flags => 1,
                                 columns => 8,
                                 rows => [{1,
                                           -30610224000000000,
                                           null,
                                           null,
                                           null,
                                           null,
                                           null,
                                           null},
                                          {2,
                                           253402300799000000,
                                           null,
                                           null,
                                           null,
                                           null,
                                           null,
                                           null}],
                                 table_id => 25,
                                 bitmap => 255},
                      footer => <<122,166,51,6>>},
          sequence => 15},
        <<62,0,0,
          15,
          0,189,12,11,101,23,100,0,0,0,61,0,0,0,82,5,0,0,0,0,
          25,0,0,0,0,0,
          1,0,8,255,252,
          1,0,0,0,0,0,0,0,140,178,66,0,0,252,2,0,0,0,0,0,0,0,
          254,243,255,126,251,122,166,51,6>>},

       {#{packet => #{header => #{flags => 0,
                                  timestamp => 1695301788,
                                  server_id => 100,
                                  event_type => write_rows_compressed_v1,
                                  event_size => 62,
                                  log_pos => 1316},
                      action => log_event,
                      event => #{flags => 1,
                                 columns => 8,
                                 rows => [{1,-30610224000000000,null,null,null,null,null,null},
                                          {2,253402300799000000,null,null,null,null,null,null}],
                                 table_id => 25,bitmap => 255},
                      footer => <<17,193,63,166>>},
          sequence => 15},
        <<63,0,0,15,0,156,64,12,101,166,100,0,0,0,62,0,0,0,36,5,0,0,0,0,25,0,0,
          0,0,0,1,0,8,255,129,28,120,156,251,195,200,0,1,61,155,156,24,24,254,
          48,65,121,255,62,255,175,251,13,0,82,169,7,229,17,193,63,166>>},

        {#{packet => #{header => #{flags => 0,
                                 timestamp => 1695222973,
                                 server_id => 100,
                                 event_type => write_rows_v1,
                                 event_size => 59,
                                 log_pos => 4376},
                     action => log_event,
                     event => #{flags => 1,
                                columns => 8,
                                rows => [{1,1000000,null,null,null,null,null,null},
                                         {2,2147483647000000,null,null,null,null,null,null}],
                                table_id => 26,
                                bitmap => 255},
                     footer => <<31,60,227,45>>},
         sequence => 49},
       <<60,0,0,49,0,189,12,11,101,23,100,0,0,0,59,0,0,0,24,17,0,0,0,0,26,0,0,
         0,0,0,1,0,8,255,252,1,0,0,0,0,0,0,0,0,0,0,1,252,2,0,0,0,0,0,0,0,127,
         255,255,255,31,60,227,45>>}]).


t0014_test_() ->
    Mapped = #{29 => #{flags => 1,
                       table => <<"t0014">>,
                       metadata => #{unsignedness => #{1 => true,
                                                       2 => false,
                                                       3 => false},
                                     column_name => [<<"i">>,
                                                     <<"q0">>,
                                                     <<"q1">>],
                                     simple_primary_key => [0]},
                       table_id => 29,
                       database => <<"test">>,
                       field_metadata => #{1 => undefined,
                                           2 => #{precision => 5,scale => 2},
                                           3 => #{precision => 14,scale => 4}},
                       coltypes => [longlong,newdecimal,newdecimal],
                       null_bitmap => <<6>>}},
    lists:map(
      t(msmp_codec:decode(
          msmp_binlog_network_stream:decode(
            #{mapped => Mapped}))),
      [{#{packet => #{header => #{flags => 0,
                                  timestamp => 1695301789,
                                  event_type => write_rows_compressed_v1,
                                  server_id => 100,
                                  event_size => 64,
                                  log_pos => 11881},
                      action => log_event,
                      event => #{flags => 1,
                                 columns => 3,
                                 rows => [{1, {-999, 99}, null},
                                          {2, {   0,  0}, null},
                                          {3, { 999, 99}, null}],
                                 table_id => 29,
                                 bitmap => 255},
                      footer => <<63,44,98,131>>},
          sequence => 139},
        <<65,0,0,139,0,157,64,12,101,166,100,0,0,0,64,0,0,0,105,46,0,0,0,0,29,0,
          0,0,0,0,1,0,3,255,129,36,120,156,251,195,200,0,1,53,18,115,254,48,65,
          217,13,12,12,127,152,161,236,230,231,201,0,113,112,6,120,63,44,98,131>>},

      {#{packet => #{header => #{flags => 0,
                                 timestamp => 1695222973,
                                 server_id => 100,
                                 event_type => write_rows_v1,
                                 event_size => 69,
                                 log_pos => 16687},
                     action => log_event,
                     event => #{flags => 1,
                                columns => 3,
                                rows => [{1, {-999, 99}, null},
                                         {2, {   0,  0}, null},
                                         {3, { 999, 99}, null}],
                                table_id => 29,
                                bitmap => 255},
                     footer => <<"Æè\"h">>},
         sequence => 139},
       <<70,0,0,139,0,189,12,11,101,23,100,0,0,0,69,0,0,0,47,65,0,0,0,0,29,0,0,
         0,0,0,1,0,3,255,252,1,0,0,0,0,0,0,0,124,24,156,252,2,0,0,0,0,0,0,0,
         128,0,0,252,3,0,0,0,0,0,0,0,131,231,99,198,232,34,104>>},

       {#{packet => #{header => #{flags => 0,
                                  timestamp => 1695222973,
                                  event_type => write_rows_v1,
                                  server_id => 100,
                                  event_size => 81,
                                  log_pos => 17013},
                      action => log_event,
                      event => #{flags => 1,
                                 columns => 3,
                                 rows => [{4, null, { 1234567890, 1234}},
                                          {5, null, {          0,    0}},
                                          {6, null, {-1234567890, 1234}}],
                                 table_id => 29,
                                 bitmap => 255},
                      footer => <<"Q¨Êf">>},
          sequence => 143},
       <<82,0,0,143,0,189,12,11,101,23,100,0,0,0,81,0,0,0,117,66,0,0,0,0,29,0,
         0,0,0,0,1,0,3,255,250,4,0,0,0,0,0,0,0,129,13,251,56,210,4,210,250,5,0,
         0,0,0,0,0,0,128,0,0,0,0,0,0,250,6,0,0,0,0,0,0,0,126,242,4,199,45,251,
         45,81,168,202,102>>}]).


t0017_test_() ->
    Mapped = #{32 => #{flags => 1,
                       table => <<"t0017">>,
                       metadata => #{unsignedness => #{1 => true},
                                     column_name => [<<"i">>,<<"q0">>],
                                     simple_primary_key => [0]},
                       database => <<"test">>,
                       field_metadata => #{1 => undefined, 2 => 6},
                       coltypes => [longlong,bit],
                       null_bitmap => <<2>>}},
    lists:map(
      t(msmp_codec:decode(
          msmp_binlog_network_stream:decode(
            #{mapped => Mapped}))),
      [{#{packet => #{header => #{flags => 0,
                                  timestamp => 1695222973,
                                  server_id => 100,
                                  event_type => write_rows_v1,
                                  event_size => 73,
                                  log_pos => 19410},
                      action => log_event,
                      event => #{flags => 1,
                                 columns => 2,
                                 rows => [{1, <<5:6>>},
                                          {2, <<0:6>>},
                                          {3, <<7:6>>},
                                          {4, <<63:6>>}],
                                 table_id => 32,
                                 bitmap => 255},
                      footer => <<"7rýê">>},
          sequence => 173},
        <<74,0,0,173,0,189,12,11,101,23,100,0,0,0,73,0,0,0,210,75,0,0,0,0,32,0,
          0,0,0,0,1,0,2,255,252,1,0,0,0,0,0,0,0,5,252,2,0,0,0,0,0,0,0,0,252,3,0,
          0,0,0,0,0,0,7,252,4,0,0,0,0,0,0,0,63,55,114,253,234>>}]).


t0018_test_() ->
    Mapped = #{33 => #{flags => 1,
                       table => <<"t0018">>,
                       metadata => #{unsignedness => #{1 => true},
                                     column_name => [<<"i">>,<<"q0">>],
                                     default_charset => ".",
                                     simple_primary_key => [0]},
                       database => <<"test">>,
                       field_metadata => #{1 => undefined,2 => 4},
                       coltypes => [longlong,blob],
                       null_bitmap => <<2>>}},
    lists:map(
      t(msmp_codec:decode(
          msmp_binlog_network_stream:decode(
            #{mapped => Mapped}))),
      [{#{packet =>
                   #{header =>
                         #{flags => 0,timestamp => 1695301789,
                           event_type => write_rows_compressed_v1,
                           server_id => 100,event_size => 146,
                           log_pos => 15612},
                     action => log_event,
                     event =>
                         #{flags => 1,columns => 2,
                           rows =>
                               [{1,<<"[\"abc\", 10, null, true, false]">>},
                                {2,<<"{\"k1\": \"value\", \"k2\": 10}">>},
                                {3,
                                 <<"[\"12:18:29.000000\", \"2015-07-29\", \"2015-07-29 12:18:29.000000\"]">>}],
                           table_id => 33,bitmap => 255},
                     footer => <<203,138,195,236>>},
               sequence => 183},
        <<147,0,0,183,0,157,64,12,101,166,100,0,0,0,146,0,0,0,252,60,0,0,0,0,33,
          0,0,0,0,0,1,0,2,255,129,157,120,156,251,195,200,0,1,114,64,28,173,148,
          152,148,172,164,163,96,104,160,163,144,87,154,147,163,163,80,82,84,
          154,170,163,144,150,152,83,156,26,251,135,9,170,84,18,136,171,149,178,
          13,149,172,20,148,202,18,115,74,83,129,90,148,178,141,128,92,67,131,
          218,63,204,80,85,246,96,3,13,141,172,12,45,172,140,44,245,12,192,0,
          164,210,200,192,208,84,215,192,92,215,200,18,149,167,128,174,54,22,0,
          233,219,31,59,203,138,195,236>>},
      {#{packet =>
                   #{header =>
                         #{flags => 0,timestamp => 1695222974,
                           event_type => write_rows_v1,server_id => 100,
                           event_size => 190,log_pos => 20440},
                     action => log_event,
                     event =>
                         #{flags => 1,columns => 2,
                           rows =>
                               [{1,<<"[\"abc\", 10, null, true, false]">>},
                                {2,<<"{\"k1\": \"value\", \"k2\": 10}">>},
                                {3,
                                 <<"[\"12:18:29.000000\", \"2015-07-29\", \"2015-07-29 12:18:29.000000\"]">>}],
                           table_id => 33,bitmap => 255},
                     footer => <<142,5,113,208>>},
               sequence => 183},
       <<191,0,0,183,0,190,12,11,101,23,100,0,0,0,190,0,0,0,216,79,0,0,0,0,33,
         0,0,0,0,0,1,0,2,255,252,1,0,0,0,0,0,0,0,30,0,0,0,91,34,97,98,99,34,44,
         32,49,48,44,32,110,117,108,108,44,32,116,114,117,101,44,32,102,97,108,
         115,101,93,252,2,0,0,0,0,0,0,0,25,0,0,0,123,34,107,49,34,58,32,34,118,
         97,108,117,101,34,44,32,34,107,50,34,58,32,49,48,125,252,3,0,0,0,0,0,
         0,0,63,0,0,0,91,34,49,50,58,49,56,58,50,57,46,48,48,48,48,48,48,34,44,
         32,34,50,48,49,53,45,48,55,45,50,57,34,44,32,34,50,48,49,53,45,48,55,
         45,50,57,32,49,50,58,49,56,58,50,57,46,48,48,48,48,48,48,34,93,142,5,
         113,208>>}]).

t0019_test_() ->
    Mapped = #{34 => #{flags => 1,
                       table => <<"t0019">>,
                       metadata => #{unsignedness => #{1 => true},
                                     column_name => [<<"i">>,<<"q0">>],
                                     simple_primary_key => [0],
                                     eunm_str_value =>
                                         <<5,7,120,45,115,109,97,108,108,5,115,109,97,108,108,6,
                                           109,101,100,105,117,109,5,108,97,114,103,101,7,120,
                                           45,108,97,114,103,101>>,
                                     enum_and_set_default_charset => <<"-">>},
                       table_id => 34,
                       database => <<"test">>,
                       field_metadata => #{1 => undefined,
                                           2 => #{length => 1, field_type => enum}},
                       coltypes => [longlong,string],
                       null_bitmap => <<2>>}},
    lists:map(
      t(msmp_codec:decode(
          msmp_binlog_network_stream:decode(
            #{mapped => Mapped}))),
          [{#{packet => #{header => #{flags => 0,
                                      timestamp => 1695476703,
                                      server_id => 100,
                                      event_type => write_rows_compressed_v1,
                                      event_size => 68,
                                      log_pos => 23127},
                          action => log_event,
                          event => #{flags => 1,
                                     columns => 2,
                                     rows => [{1,1}, {2,2}, {3,3}, {4,4}, {5,5}],
                                     table_id => 34,
                                     bitmap => 255},
                          footer => <<"¦\vÊi">>},
              sequence => 33},
            <<69,0,0,33,0,223,235,14,101,166,100,0,0,0,68,0,0,0,87,90,0,0,0,0,34,0,
              0,0,0,0,1,0,2,255,129,50,120,156,251,195,200,0,1,140,127,152,160,44,
              166,63,204,80,22,243,31,22,40,139,229,15,43,148,197,10,0,150,0,5,11,
              166,11,202,105>>}]).


world_cup_teams_2022_test_() ->
    Mapped = #{118 => #{flags => 1,
                        table => <<"world_cup_teams_2022">>,
                        metadata => #{simple_primary_key => [0],
                                      column_name => [<<"country">>,
                                                      <<"top_searched">>,
                                                      <<"second">>,
                                                      <<"third">>,
                                                      <<"fourth">>,
                                                      <<"fifth">>],
                                      default_charset => "ÿ",
                                      column_visibility => <<"ü">>},
                        table_id => 118,
                        database => <<"shortishly">>,
                        field_metadata => #{1 => 200,
                                            2 => 2,
                                            3 => 2,
                                            4 => 2,
                                            5 => 2,
                                            6 => 2},
                        coltypes => [varchar,
                                     blob,
                                     blob,
                                     blob,
                                     blob,
                                     blob],
                        null_bitmap => <<">">>}},
    t(msmp_codec:decode(
        msmp_binlog_network_stream:decode(
          #{mapped => Mapped})),
      "test/write-rows-world-cup-teams-2022.terms").
