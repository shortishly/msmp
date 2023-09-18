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


-module(msmp_handshake_response_tests).


-import(msmp_tests, [t/1]).
-import(msmp_tests, [t/2]).
-include_lib("eunit/include/eunit.hrl").


decode_test_() ->
    t(msmp_codec:decode(
        msmp_handshake_response:decode()),
      "test/handshake-response.terms").


encode_test_() ->
    lists:map(
      t(fun
            (Decoded) ->
                Encode = (msmp_codec:encode(msmp_handshake_response:encode())),
                Decode = (msmp_codec:decode(msmp_handshake_response:decode())),
                {<<>>, Result} = Decode(iolist_to_binary(Encode(Decoded))),
                Result
        end),
      [{match, Decoded, Decoded} ||
          {Decoded, _Encoded} <- phrase_file:consult(
                                   "test/handshake-response.terms")]).


auth_response_test_() ->
    lists:map(

      t(fun msmp_handshake_response:auth_response/2),

      [{<<121,48,154,105,171,193,214,114,240,79,37,59,150,106,48,29,211,
          252,169,254>>,
        [#{auth_plugin_name => mysql_native_password,
           auth_plugin_data_part_1 => <<18,64,20,6,98,111,108,76>>,
           auth_plugin_data_part_2 => <<12,10,22,9,69,89,103,64,113,22,75,91,0>>},
         "secret"]},

       {<<100,50,25,190,61,238,57,93,253,200,139,98,74,239,45,143,227,101,
          238,54>>,
        [#{auth_plugin_name => mysql_native_password,
           auth_plugin_data_part_1 => <<"%m1xl\e<\v">>,
           auth_plugin_data_part_2 => <<19,126,19,43,124,59,4,49,52,92,52,57,0>>},
         "secret"]},

       {<<69,141,33,190,107,186,56,64,54,147,2,199,90,135,117,
          38,178,52,105,225,195,186,202,11,126,22,82,218,164,
          225,146,225>>,
        [#{auth_plugin_data_part_1 => <<25,8,71,123,107,99,33,108>>,
           auth_plugin_data_part_2 => <<90,62,37,105,96,22,99,101,63,78,69,91,0>>,
           auth_plugin_name => caching_sha2_password},
         "secret"]}]).
