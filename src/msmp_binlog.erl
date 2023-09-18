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


-module(msmp_binlog).


-export([decode/0]).
-import(scran_bytes, [tag/1]).
-import(scran_sequence, [preceded/2]).
-import(scran_multi, [many1/1]).


decode() ->
    fun
        (Input) ->
            (preceded(
               header(),
               many1(msmp_binlog_event:decode())))(Input)
    end.


header() ->
    fun
        (Input) ->
            (tag(<<16#fe, "bin">>))(Input)
    end.
