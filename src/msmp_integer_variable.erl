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

%% @doc An unsigned variable length little endian integer.

-module(msmp_integer_variable).


-export([decode/0]).
-export([encode/0]).


-type variable_length() :: msmp:u1()
                         | msmp:u2()
                         | msmp:u3()
                         | msmp:u8().


%% @doc Decode an unsigned variable length little endian integer.
-spec decode() -> scran:parser(binary(), variable_length()).

decode() ->
    fun
        (<<I:8, Remaining/bytes>>) when I < 251 ->
            {Remaining, I};

        (<<16#fc, I:16/little, Remaining/bytes>>) when I >= 251, I < 1 bsl 16 ->
            {Remaining, I};

        (<<16#fd, I:24/little, Remaining/bytes>>) when I >= 1 bsl 16, I < 1 bsl 24 ->
            {Remaining, I};

        (<<16#fe, I:64/little, Remaining/bytes>>) when I >= 1 bsl 24, I < 1 bsl 64 ->
            {Remaining, I};

        (_) ->
            nomatch
    end.


%% @doc Encode an unsigned variable length little endian integer.
-spec encode() -> narcs:encoder(non_neg_integer(), binary()).

encode() ->
    fun
        (I) when I < 251 ->
            <<I:8>>;

        (I) when I >= 251, I < 1 bsl 16 ->
            <<16#fc, I:16/little>>;

        (I) when I >= 1 bsl 16, I < 1 bsl 24 ->
            <<16#fd, I:24/little>>;

        (I) when I >= 1 bsl 24, I < 1 bsl 64 ->
            <<16#fe, I:64/little>>
    end.
