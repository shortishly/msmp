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


-module(msmp_com_query_tests).


-include_lib("eunit/include/eunit.hrl").
-import(msmp_tests, [t/2]).


decode_test_() ->
    t(msmp_codec:decode(
        msmp_com_query:decode(
          #{query_attributes => true})),
      "test/com-query.terms").

decode_na_test_() ->
    t(msmp_codec:decode(
        msmp_com_query:decode(
          #{query_attributes => false})),
      "test/com-query-na.terms").
