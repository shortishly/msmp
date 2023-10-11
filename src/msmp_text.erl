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


-module(msmp_text).

-feature(maybe_expr, enable).

-export([decode/1]).
-export([encode/1]).


decode(#{type := Type})
  when Type == long;
       Type == longlong;
       Type == short;
       Type == year;
       Type == tiny ->
    decoder(fun erlang:binary_to_integer/1);

decode(#{type := Type})
  when Type == float;
       Type == double;
       Type == decimal;
       Type == newdecimal ->
    decoder(fun erlang:binary_to_float/1);

decode(#{type := Type})
  when Type == string;
       Type == var_string;
       Type == varchar;
       Type == long_blob;
       Type == blob ->
    fun
        (Encoded) ->
            {<<>>, Encoded}
    end.


decoder(Decoder) ->
    fun
        (null) ->
            {<<>>, null};

        (Encoded) ->
            {<<>>, Decoder(Encoded)}
    end.


encode(#{type := Type})
  when Type == long;
       Type == longlong;
       Type == short;
       Type == year;
       Type == tiny ->
    fun erlang:integer_to_binary/1;

encode(#{type := Type})
  when Type == float;
       Type == double;
       Type == decimal;
       Type == newdecimal ->
    fun
        (Decoded) ->
            float_to_binary(Decoded, [short])
    end;

encode(#{type := Type})
  when Type == string;
       Type == var_string;
       Type == varchar;
       Type == long_blob;
       Type == blob ->
    fun
        (Encoded) ->
            Encoded
    end.
