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


-module(msmp_register_replica_tests).


-import(msmp_tests, [t/1]).
-import(msmp_tests, [t/2]).
-include_lib("eunit/include/eunit.hrl").


decode_test_() ->
    t(msmp_codec:decode(msmp_register_replica:decode()),
      "test/register-replica.terms").


encode_test_() ->
    lists:map(
      t(narcs_result:to_binary(
          msmp_codec:encode(
            msmp_register_replica:encode()))),
      [{Encoded, Decoded} || {Decoded, Encoded} <- phrase_file:consult("test/register-replica.terms")]).
