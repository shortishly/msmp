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


-module(msmp_binary_resultset_row_tests).


-import(msmp_tests, [t/1]).
-include_lib("eunit/include/eunit.hrl").


hypotenuse_test_() ->
    Definitions = [#{decimals => 31,
                    flags => 128,
                    name => <<"hypotenuse">>,
                    table => <<>>,
                    type => double,
                    action => column_definition,
                    character_set => 63,
                    catalog => <<"def">>,
                    schema => <<>>,
                    org_table => <<>>,
                    org_name => <<>>,
                    length_of_fixed_length_fields => 12,
                    column_length => 23,
                    reserved0 => <<0,0>>}],
    lists:map(
      t(msmp_codec:decode(
          msmp_binary_resultset_row:decode(Definitions))),
      [{#{packet => #{action => binary_resultset_row,
                     row => [0.0],
                     null_bitmap => [false]},
         sequence => 3},
        <<10,0,0,
          3,
          0, % packet header
          0, % null bitmap
          0,0,0,0,0,0,0,0>>}]).


cast_2_dot_2_as_double_test_() ->
    Definitions = [#{decimals => 31,
                     flags => #{binary => true,
                                set => false,
                                timestamp => false,
                                enum => false,
                                group => false,
                                unique => false,
                                unsigned => false,
                                blob => false,
                                not_null => true,
                                primary_key => false,
                                unique_key => false,
                                multiple_key => false,
                                zerofill => false,
                                auto_increment => false,
                                no_default_value => false,
                                on_update_now => false,
                                part_key => false},
                     name => <<"cast(2.2 as double)">>,
                     table => <<>>,
                     type => double,
                     action => column_definition,
                     character_set => 63,
                     reserved0 => <<0,0>>,
                     catalog => <<"def">>,
                     schema => <<>>,
                     org_table => <<>>,
                     org_name => <<>>,
                     length_of_fixed_length_fields => 12,
                     column_length => 23}],
    lists:map(
      t(msmp_codec:decode(
          msmp_binary_resultset_row:decode(Definitions))),
      [{#{packet => #{action => binary_resultset_row,
                     row => [2.2],
                     null_bitmap => [false]},
         sequence => 3},
        <<10,0,0,
          3, % sequence number
          0, % packet header
          0, % null bitmap
          154,153,153,153,153,153,1,64>>}]).

var_string_test_() ->
    Definitions = [#{decimals => 31,
                     flags => #{binary => false,
                                set => false,
                                timestamp => false,
                                enum => false,
                                group => false,
                                unique => false,
                                unsigned => false,
                                blob => false,
                                not_null => false,
                                primary_key => false,
                                unique_key => false,
                                multiple_key => false,
                                zerofill => false,
                                auto_increment => false,
                                no_default_value => false,
                                on_update_now => false,
                                part_key => false},
                     name => <<"cast('abc' as char)">>,
                     table => <<>>,
                     type => var_string,
                     action => column_definition,
                     character_set => 255,
                     reserved0 => <<0,0>>,
                     catalog => <<"def">>,
                     schema => <<>>,
                     org_table => <<>>,
                     org_name => <<>>,
                     length_of_fixed_length_fields => 12,
                     column_length => 12}],
    lists:map(
      t(msmp_codec:decode(
          msmp_binary_resultset_row:decode(Definitions))),
      [{#{packet => #{action => binary_resultset_row,
                      null_bitmap => [false],
                      row => [<<"abc">>]},
          sequence => 3},
        <<6,0,0,3,0,0,3,97,98,99>>}]).



new_decimal_test_() ->
    Definitions = [#{decimals => 2,
                     flags => #{binary => true,
                                set => false,
                                timestamp => false,
                                enum => false,
                                group => false,
                                unique => false,
                                unsigned => false,
                                blob => false,
                                not_null => true,
                                primary_key => false,
                                unique_key => false,
                                multiple_key => false,
                                zerofill => false,
                                auto_increment => false,
                                no_default_value => false,
                                on_update_now => false,
                                part_key => false},
                     name => <<"cast(2.2 as decimal(5,2))">>,
                     table => <<>>,
                     type => newdecimal,
                     action => column_definition,
                     character_set => 63,
                     reserved0 => <<0,0>>,
                     catalog => <<"def">>,
                     schema => <<>>,
                     org_table => <<>>,
                     org_name => <<>>,
                     length_of_fixed_length_fields => 12,
                     column_length => 7}],
    lists:map(
      t(msmp_codec:decode(
          msmp_binary_resultset_row:decode(Definitions))),
      [{#{packet => #{action => binary_resultset_row,
                     row => [2.2],
                     null_bitmap => [false]},
         sequence => 3},
        <<7,0,0,
          3,
          0,
          0,
          4,50,46,50,48>>}]).
