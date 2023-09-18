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


-module(ssl_request_tests).


-include_lib("eunit/include/eunit.hrl").


decode_test_() ->
    lists:map(
      decode(msmp_codec:decode(msmp_ssl_request:decode())),
      begin
          {ok, Tests} = file:consult("test/ssl-request.terms"),
          Tests
      end).


decode(F) ->
    fun
        ({Expected, Input} = Test) ->
            {nm(Test),
             ?_assertEqual(Expected,
                           case F(Input) of

                               {<<>>, Expected} ->
                                   Expected;

                               Otherwise ->
                                   ?debugVal(Otherwise, -1),
                                   Otherwise
                           end)}
    end.


nm(Test) ->
    iolist_to_binary(io_lib:fwrite("~p", [Test])).
