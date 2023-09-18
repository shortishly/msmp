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

-module(msmp_jsonb_tests).


-import(msmp_tests, [t/1]).
-include_lib("eunit/include/eunit.hrl").


type_test_() ->
    lists:map(
      t(msmp_jsonb:type(string)),
      [{{<<"abc">>, <<12>>}, <<12, "abc">>},
       {nomatch, <<6, "abc">>}]).


decode_test_() ->
    lists:map(
      fun
          ({Expected, Input, Metadata} = Test) ->
              {iolist_to_binary(io_lib:fwrite("~p", [Test])),
               ?_assertEqual(
                  Expected,
                  case (msmp_jsonb:decode(Metadata))(Input) of
                      {<<>>, Expected} ->
                          Expected;

                      Otherwise ->
                          ?debugVal(Otherwise, -1),
                          Otherwise
                  end)}
      end,
      phrase_file:consult("test/jsonb.terms")).
