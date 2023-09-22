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


-module(msmp_binlog_delete_rows_tests).


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
                                  event_type => delete_rows,
                                  event_size => 45,
                                  log_pos => 3403579},
                      action => log_event,
                      event => #{flags => 1,
                                 rows => [{1,0}],
                                 columns => 2,
                                 bitmap => 255,
                                 table_id => 122,
                                 extra_row_info => <<>>},
                      footer => <<0,44,117,68>>},
               sequence => 212},
        <<46,0,0,212,0,145,60,210,100,32,100,0,0,0,45,0,0,0,59,239,51,0,0,0,122,
          0,0,0,0,0,1,0,2,0,2,255,0,1,0,0,0,0,0,0,0,0,0,44,117,68>>},

       {#{packet => #{header => #{flags => 0,
                                  timestamp => 1695301789,
                                  event_type => delete_rows_compressed_v1,
                                  server_id => 100,
                                  event_size => 47,
                                  log_pos => 33395},
                      action => log_event,
                      event => #{flags => 1,
                                 columns => 2,
                                 rows => [{null,10},{648523847267777436,-21}],
                                 table_id => 122,
                                 bitmap => 255},
                      footer => <<"O}ø\v">>},
          sequence => 167},
        <<48,0,0,167,0,157,64,12,101,168,100,0,0,0,47,0,0,0,115,130,0,0,0,0,122,
          0,0,0,0,0,1,0,2,255,129,10,120,156,251,195,200,0,5,0,9,235,0,254,79,
          125,248,11>>}]).
