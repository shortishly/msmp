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


-module(msmp_com_stmt_close).


-export([encode/1]).


command() ->
    <<16#19>>.


encode(_ClientFlags) ->
    fun
        (Decoded) ->
            (narcs_sequence:sequence(
               [narcs_bytes:tag(command()),
                narcs_combinator:v(
                  statement_id,
                  msmp_integer_fixed:encode(4))]))(Decoded)
    end.
