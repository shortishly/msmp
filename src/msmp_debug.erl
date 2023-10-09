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


-module(msmp_debug).


-feature(maybe_expr, enable).


-export([binary_workflow/1]).
-export([packetise/2]).
-export([unhex/2]).
-export([write_file/2]).
-import(scran_branch, [alt/1]).
-import(scran_bytes, [length_encoded/1]).
-import(scran_combinator, [map_parser/2]).
-import(scran_combinator, [map_result/2]).
-import(scran_combinator, [peek/1]).
-import(scran_combinator, [rest/0]).
-import(scran_result, [into_map/1]).
-import(scran_result, [kv/2]).
-import(scran_sequence, [sequence/1]).
-include_lib("kernel/include/logger.hrl").


dump(Parser) ->
    fun
        (Input) ->
            (into_map(
               map_parser(
                 length_encoded(
                   map_result(
                     msmp_integer_fixed:decode(3),
                     fun
                         (Length) ->
                             %% adjust length to include the sequence number
                             Length + 1
                     end)),
                 sequence(
                   [kv(encoded,
                       map_result(
                         peek(rest()),
                         fun
                             (<<Sequence:8, Body/bytes>>) ->
                                 ?LOG_DEBUG(#{size => byte_size(Body),
                                              sequence => Sequence,
                                              packet => <<(byte_size(Body)):24/little, Sequence:8, Body/bytes>>}),
                                 <<(byte_size(Body)):24/little, Sequence:8, Body/bytes>>
                         end)),
                    kv(sequence, msmp_integer_fixed:decode(1)),
                    kv(packet, Parser)]))))(Input)
    end.

parser(BinLog, ClientFlags) ->
    fun
        (Input) ->
            (alt([msmp_binlog_network_stream:decode(BinLog),
                  msmp_binlog_dump:decode(),
                  msmp_binlog_dump_gtid:decode(),
                  msmp_handshake:decode(),
                  msmp_handshake_response:decode(),
                  msmp_packet_ok:decode(ClientFlags),
                  msmp_packet_error:decode(ClientFlags),
                  msmp_com_query:decode(ClientFlags),
                  msmp_packet_eof:decode(ClientFlags),
                  msmp_ssl_request:decode(),
                  %%                  msmp_auth_more_data:decode(),
                  msmp_register_replica:decode(),
                  otherwise()]))(Input)
    end.

otherwise() ->
    fun
        (Input) ->
            (into_map(sequence([kv(otherwise, rest())])))(Input)
    end.


repeated(BinLog, ClientFlags) ->
    fun
        (Input) ->
            (?FUNCTION_NAME(BinLog, ClientFlags, []))(Input)
    end.


repeated(#{mapped := Mapped} = BinLog, ClientFlags, A) ->
    fun
        (Input) ->
            %% ?LOG_DEBUG(#{input => Input, a_length => length(A)}),

            case (dump(parser(BinLog, ClientFlags)))(Input) of
                {Remaining,
                 #{packet := #{action := handshake,
                               capability_flags_1 := Flags1,
                               capability_flags_2 := Flags2}} = Packet} ->
                    ?LOG_DEBUG(#{packet => Packet}),
                    (?FUNCTION_NAME(
                        BinLog,
                        maps:merge(Flags1, Flags2),
                        [Packet | A]))
                      (Remaining);

                {Remaining,
                 #{packet := #{action := handshake_response,
                               client_flags := Flags}} = Packet} ->
                    ?LOG_DEBUG(#{packet => Packet}),
                    (?FUNCTION_NAME(
                        BinLog,
                        Flags,
                        [Packet | A]))
                      (Remaining);

                {Remaining,
                 #{packet := #{action := log_event,
                               header := #{event_type := table_map},
                               event := #{table_id := TableId} = Event}} = Packet} ->
                    ?LOG_DEBUG(#{packet => Packet}),
                    (?FUNCTION_NAME(
                        BinLog#{mapped := Mapped#{TableId => maps:without([table_id], Event)}},
                        ClientFlags,
                        [Packet | A]))
                      (Remaining);

                {Remaining, #{} = Packet}  ->
                    ?LOG_DEBUG(#{packet => Packet}),
                    (?FUNCTION_NAME(
                        BinLog,
                        ClientFlags,
                        [Packet | A]))
                      (Remaining);

                nomatch when A == [] ->
                    nomatch;

                nomatch ->
                    {Input, lists:reverse(A)}
            end
    end.


write_file(In, Out) ->
    {ok, Packets} = file:consult(In),

    ClientFlags = #{protocol_41 => true,
                    session_track => true,
                    transactions => true},

    BinLog = #{mapped => #{}},

    {<<>>, Decoded} = (repeated(
                         BinLog,
                         ClientFlags))
                        (iolist_to_binary(
                           [Data || {frame, Data} <- Packets])),
    {ok, Header} = file:read_file("HEADER.terms"),
    ok = write_terms(Out, Header, Decoded).


write_terms(Filename, Header, Terms) ->
    file:write_file(
      Filename,
      [Header,
       "\n",
       lists:map(
         fun
             (Term) ->
                 io_lib:format("~p.~n~n", [Term])
         end,
         Terms)]).


packetise(In, Out) ->
    {ok, Packets} = file:consult(In),
    {ok, Header} = file:read_file("HEADER.terms"),

    write_terms(
      Out,
      Header,
      lists:reverse(
        frames(iolist_to_binary(
                 lists:filtermap(
                   fun
                       (#{packet := #{ip := #{tcp := #{data := Data}}}}) when Data /= <<>> ->
                           {true, Data};

                       (_) ->
                           false
                   end,
                   Packets))))).


unhex(In, Out) ->
    {ok, Hexed} = file:read_file(In),
    {ok, Header} = file:read_file("HEADER.terms"),
    write_terms(
      Out,
      Header,
      lists:reverse(
        frames(
          iolist_to_binary(
            [binary:decode_hex(Hex) || Hex <- binary:split(Hexed, <<"\n">>, [global,trim_all])])))).

binary_workflow(In) ->
    ok = unhex(In ++ ".binary", In ++ ".terms"),
    ok = write_file(In ++ ".terms", In ++ "-msmp-debug.terms").


frames(Frames) ->
    ?FUNCTION_NAME(Frames, []).

frames(<<Length:24/little, SeqBody:(Length + 1)/bytes, Remainder/bytes>>, A) ->
    ?FUNCTION_NAME(Remainder,
                   [{frame, <<Length:24/little, SeqBody:(Length + 1)/bytes>>} | A]);
frames(<<>>, A) ->
    A;

frames(Remainder, A) ->
    [{remaining, Remainder} | A].
