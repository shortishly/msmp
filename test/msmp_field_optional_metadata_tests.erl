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


-module(msmp_field_optional_metadata_tests).


-include_lib("eunit/include/eunit.hrl").


signedness_test() ->
    ?assertEqual(
       {<<>>, #{unsignedness => #{1 => true, 2 => false, 3 => true}}},
       (msmp_field_optional_metadata:decode([longlong, tiny, tiny]))(<<1, 1, 160>>)).


signedness_with_default_charset_test() ->
    ?assertEqual(
       {<<>>,
        #{unsignedness => #{1 => false},
          default_charset => [255]}},
       (msmp_field_optional_metadata:decode(
          [long,varchar,date,time2,datetime2]))
         (<<1,1,0,2,3,252,255,0>>)).
