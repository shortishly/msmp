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


-module(msmp_auth_more_data).


-export([decode/0]).

-spec decode() -> scran:parser(binary(), msmp:auth_more_data()).

decode() ->
    fun
        (Input) ->
            (scran_result:into_map(
               scran_sequence:sequence(
                 [scran_result:kv(
                    action,
                    scran_combinator:value(
                      auth_more_data,
                      scran_bytes:tag(<<1>>))),

                  scran_branch:alt(
                    [scran_sequence:sequence(
                       [scran_result:kv(
                          status,
                          scran_combinator:value(
                            fast_auth_success,
                            scran_bytes:tag(<<3>>))),
                        scran_combinator:ignore_result(
                          scran_combinator:eof())]),

                     scran_sequence:sequence(
                       [scran_result:kv(
                          status,
                          scran_combinator:value(
                            perform_full_authentication,
                            scran_bytes:tag(<<4>>))),
                        scran_combinator:ignore_result(
                          scran_combinator:eof())]),

                     %% if not otherwise tagged the more auth data
                     %% must be a server public key transmitted in
                     %% clear text.
                     scran_result:kv(
                       public_key,
                       scran_combinator:rest())])])))(Input)
    end.
