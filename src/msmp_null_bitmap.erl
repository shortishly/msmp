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


-module(msmp_null_bitmap).


-export([decode/1]).
-export([decode/2]).
-export([encode/0]).
-export([encode/1]).


encode() ->
    ?FUNCTION_NAME(0).


encode(Offset) ->
    fun
        (Decoded) ->
            Padding = padding(length(Decoded), Offset),

            (narcs_combinator:map_result(
               msmp_narcs:sequence(
                 [narcs_bytes:tag(<<0:Offset>>),
                  msmp_narcs:many0(
                    narcs_combinator:map_result(
                      is_null(),
                      narcs_bits:into_bit())),
                  narcs_bytes:tag(<<0:Padding>>)]),
              fun erlang:list_to_bitstring/1))(Decoded)
    end.


is_null() ->
    fun
        (null) ->
            true;

        (_) ->
            false
    end.


padding(N, Offset) ->
    width(N, Offset) - Offset - N.


width(N, Offset) ->
   ((N + 7 + Offset) div 8) * 8.


decode(N) ->
    ?FUNCTION_NAME(N, 0).

decode(N, Offset) ->
    Padding = padding(N, Offset),
    fun
        (<<_:Offset/bits, Bitmap:N/bits, 0:Padding, Remaining/bytes>>) ->
            {Remaining, [tof(Bit) || <<Bit:1>> <= Bitmap]};

        (_) ->
            nomatch
    end.


tof(1) ->
    true;
tof(0) ->
    false.
