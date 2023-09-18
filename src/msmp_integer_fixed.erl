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

%% @doc An unsigned integer represented by a fixed number of little
%% endian bytes.

-module(msmp_integer_fixed).


-export([decode/1]).
-export([encode/1]).


%% @doc Decode a fixed size little endian unsigned integer.
-spec decode(pos_integer()) -> scran:parser(binary(), non_neg_integer()).

decode(Bytes) ->
    scran_number:u(little, Bytes * 8).


%% @doc Encode a fixed size little endian unsigned integer.
-spec encode(pos_integer()) -> narcs:encoder(non_neg_integer(), binary()).

encode(Bytes) ->
    narcs_number:u(little, Bytes * 8).
