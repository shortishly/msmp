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


-module(msmp_binary).


-export([decode/1]).
-export([encode/1]).
-export([null_bitmap_bytes/2]).


null_bitmap_bytes(NumFields, Offset) ->
    (NumFields + 7 + Offset) div 8.


decode(#{type := var_string}) ->
    msmp_string_length_encoded:decode();

decode(#{type := newdecimal}) ->
    scran_combinator:map_result(
      msmp_string_length_encoded:decode(),
      fun binary_to_float/1);

decode(#{type := tiny, flags := #{unsigned := true}}) ->
    scran_number:u(little, 8);

decode(#{type := tiny}) ->
    scran_number:i(little, 8);

decode(#{type := short, flags := #{unsigned := true}}) ->
    scran_number:u(little, 16);

decode(#{type := short}) ->
    scran_number:i(little, 16);

decode(#{type := long, flags := #{unsigned := true}}) ->
    scran_number:u(little, 32);

decode(#{type := long}) ->
    scran_number:i(little, 32);

decode(#{type := longlong, flags := #{unsigned := true}}) ->
    scran_number:u(little, 64);

decode(#{type := longlong}) ->
    scran_number:i(little, 64);

decode(#{type := datetime}) ->
    scran_bytes:take(5);

decode(#{type := float}) ->
    scran_number:f(little, 32);

decode(#{type := double}) ->
    scran_number:f(little, 64).


encode(#{type := tiny, flags := #{unsigned := true}}) ->
    narcs_number:u(little, 8);

encode(#{type := tiny}) ->
    narcs_number:i(little, 8);

encode(#{type := short, flags := #{unsigned := true}}) ->
    narcs_number:u(little, 16);

encode(#{type := short}) ->
    narcs_number:i(little, 16);

encode(#{type := long, flags := #{unsigned := true}}) ->
    narcs_number:u(little, 32);

encode(#{type := long}) ->
    narcs_number:i(little, 32);

encode(#{type := longlong, flags := #{unsigned := true}}) ->
    narcs_number:u(little, 64);

encode(#{type := longlong}) ->
    narcs_number:i(little, 64);

encode(#{type := float}) ->
    narcs_number:f(little, 32);

encode(#{type := double}) ->
    narcs_number:f(little, 64).
