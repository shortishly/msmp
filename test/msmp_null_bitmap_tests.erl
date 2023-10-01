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


-module(msmp_null_bitmap_tests).


-include_lib("eunit/include/eunit.hrl").


encode_col4_offset0_test() ->
    Decoded = [null, abc, null, null],
    ?assertEqual(
       <<1:1, 0:1, 1:1, 1:1, 0:4>>,
       (msmp_null_bitmap:encode())(Decoded)).

encode_col4_offset2_test() ->
    Decoded = [null, abc, null, null],
    ?assertEqual(
       <<0:2, 1:1, 0:1, 1:1, 1:1, 0:2>>,
       (msmp_null_bitmap:encode(2))(Decoded)).

decode_col1_offset2_test() ->
    Encoded = <<0:8>>,
    Decoded = [false],
    ?assertEqual(
       {<<>>, Decoded},
       (msmp_null_bitmap:decode(1, 2))(Encoded)).

decode_col4_offset0_test() ->
    Encoded = <<1:1, 0:1, 1:1, 1:1, 0:4>>,
    Decoded = [true, false, true, true],
    ?assertEqual(
       {<<>>, Decoded},
       (msmp_null_bitmap:decode(4))(Encoded)).

decode_col4_offset2_test() ->
    Encoded = <<0:2, 1:1, 0:1, 1:1, 1:1, 0:2>>,
    Decoded = [true, false, true, true],
    ?assertEqual(
       {<<>>, Decoded},
       (msmp_null_bitmap:decode(4, 2))(Encoded)).
