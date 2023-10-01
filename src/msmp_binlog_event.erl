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


%% @doc Replication protocol events


-module(msmp_binlog_event).


-feature(maybe_expr, enable).


-define(FL_COMPLETED_XA, 128).
-define(FL_GROUP_COMMIT_ID, 2).
-define(FL_PREPARED_XA, 64).
-export([decode/0]).
-export([decode/1]).
-export([rows/1]).
-import(scran_bits, [into_boolean/0]).
-import(scran_bytes, [length_encoded/1]).
-import(scran_bytes, [tag/1]).
-import(scran_bytes, [take/1]).
-import(scran_combinator, [condition/2]).
-import(scran_combinator, [condition/3]).
-import(scran_combinator, [map_parser/2]).
-import(scran_combinator, [map_result/2]).
-import(scran_combinator, [rest/0]).
-import(scran_combinator, [success/1]).
-import(scran_multi, [count/2]).
-import(scran_multi, [many1/1]).
-import(scran_result, [ignore/1]).
-import(scran_result, [into_bits/2]).
-import(scran_result, [into_map/1]).
-import(scran_result, [into_tuple/1]).
-import(scran_result, [kv/2]).
-import(scran_sequence, [combined_with/2]).
-import(scran_sequence, [followed_with/2]).
-import(scran_sequence, [pair/2]).
-import(scran_sequence, [sequence/1]).
-include_lib("kernel/include/logger.hrl").
-on_load(on_load/0).


on_load() ->
    persistent_term:put(
      ?MODULE,
      msmp_enum:priv_consult("event-type.terms")).


decode() ->
    ?FUNCTION_NAME(#{mapped => #{}}).


-type binlog_event() :: #{action := log_event,
                          header := header(),
                          event := event_body(),
                          footer := binary()}.

-spec decode(map()) -> scran:parser(binary(), binlog_event()).

decode(Arg) ->
    fun
        (Input) ->
            (followed_with(
               header(Arg),
               fun
                   (#{event_size := EventSize} = Header) ->
                       into_map(
                         sequence(
                           [kv(action, success(log_event)),
                            kv(header, success(Header)),
                            kv(event,
                               map_parser(
                                 scran_bytes:take(
                                   EventSize - header_size(Arg) - 4),
                                 event(Arg, Header))),
                            kv(footer, rest())]))
               end))(Input)
    end.


-type header() :: #{timestamp := msmp:u4(),
                    event_type := event_type(),
                    server_id := msmp:u4(),
                    event_size := msmp:u4(),
                    log_pos => msmp:u4(),
                    flags => msmp:u4()}.

-spec header(map()) -> scran:parser(binary(), header()).

header(Arg) ->
    fun
        (Input) ->
            (into_map(
               sequence(
                 [kv(timestamp, msmp_integer_fixed:decode(4)),
                  kv(event_type, event_type()),
                  kv(server_id, msmp_integer_fixed:decode(4)),
                  kv(event_size, msmp_integer_fixed:decode(4)),
                  condition(
                    header_size(Arg) > 1,
                    sequence(
                      [kv(log_pos, msmp_integer_fixed:decode(4)),
                       kv(flags, msmp_integer_fixed:decode(2))]))])))(Input)
    end.

-type event_type() :: start_v3
                    | query
                    | stop
                    | rotate
                    | intvar
                    | slave
                    | append_block
                    | delete_file
                    | rand
                    | user_var
                    | format_description
                    | xid
                    | begin_load_query
                    | execute_load_query
                    | table_map
                    | write_rows_v1
                    | update_rows_v1
                    | delete_rows_v1
                    | incident_event
                    | heartbeat_log
                    | ignorable_log
                    | rows_query_log
                    | write_rows
                    | update_rows
                    | delete_rows
                    | gtid_log
                    | anonymous_gtid_log
                    | previous_gtids_log
                    | transaction_context
                    | view_change
                    | xa_prepare_log
                    | partial_update_rows
                    | transaction_payload
                    | heartbeat.

-spec event_type() -> scran:parser(binary(), event_type()).

event_type() ->
    fun
        (Input) ->
            (map_result(msmp_integer_fixed:decode(1), fun event_type/1))(Input)
    end.


event_type(Type) ->
    maps:get(Type,
             persistent_term:get(?MODULE),
             {unknown, Type}).


-type event_body() :: rotate()
                    | format_description()
                    | table_map().

-type rotate() :: #{event_type := rotate,
                    position => msmp:u8(),
                    new_log_ident => binary()}.

-type format_description() :: #{binlog_version := msmp:u2(),
                                mysql_server_version := binary(),
                                create_timestamp := msmp:u4(),
                                header_length := msmp:u1(),
                                stuff := binary()}.

-type table_map() :: #{event_type := table_map,
                       table_id := msmp:u6(),
                       flags := msmp:u2(),
                       database := binary(),
                       coltypes := [msmp_field:type()],
                       metadata := msmp_field_optional_metadata:metadata(),
                       table := binary()}.


event(Arg, #{event_type := rotate}) ->
    fun
        (Input) ->
            (into_map(
               sequence(
                 [condition(
                    version(Arg) > 1,
                    kv(position, msmp_integer_fixed:decode(8))),
                  kv(new_log_ident, msmp_string_rest_of_packet:decode())])))(Input)
    end;

event(_, #{event_type := format_description}) ->
    fun
        (Input) ->
            (into_map(
               sequence(
                 [kv(binlog_version, msmp_integer_fixed:decode(2)),
                  kv(mysql_server_version,
                     map_parser(
                       msmp_string_fixed:decode(50),
                       msmp_string_null_terminated:decode())),
                  kv(create_timestamp, msmp_integer_fixed:decode(4)),
                  kv(header_length, msmp_integer_fixed:decode(1)),
                  kv(stuff, rest())])))(Input)
    end;

event(_, #{event_type := table_map}) ->
    fun
        (Input) ->
            (combined_with(
               map_result(
                 into_map(
                   sequence(
                     [kv(table_id, msmp_integer_fixed:decode(6)),
                      kv(flags, msmp_integer_fixed:decode(2)),
                      kv(database, msmp_string_length_encoded:decode()),
                      ignore(tag(<<0>>)),
                      kv(table, msmp_string_length_encoded:decode()),
                      ignore(tag(<<0>>)),
                      kv(coltypes,
                         count(
                           msmp_integer_variable:decode(),
                           map_result(
                             msmp_integer_fixed:decode(1),
                             fun msmp_field:lookup/1))),
                      kv(field_metadata, take(msmp_integer_variable:decode()))])),
                 fun
                     (#{field_metadata := FieldMetadata,
                        coltypes := ColTypes} = Result) ->
                         Result#{field_metadata := field_metadata(
                                                     ColTypes,
                                                     FieldMetadata)}
                 end),
               fun
                   (#{coltypes := ColTypes}) ->
                       into_map(
                         sequence(
                           [kv(null_bitmap,
                               take(bitmap_bytes(length(ColTypes)))),
                            kv(metadata,
                               msmp_field_optional_metadata:decode(ColTypes))]))
               end))(Input)
    end;

event(#{mapped := Mapped}, #{event_type := EventType})
  when EventType == update_rows;
       EventType == update_rows_v1;
       EventType == update_rows_compressed_v1;
       EventType == write_rows;
       EventType == write_rows_v1;
       EventType == write_rows_compressed_v1;
       EventType == delete_rows;
       EventType == delete_rows_v1;
       EventType == delete_rows_compressed_v1->
    fun
        (Input) ->
            (followed_with(
               msmp_integer_fixed:decode(6),

               fun
                   (TableId) when is_map_key(TableId, Mapped) ->

                       #{TableId := #{coltypes := ColTypes} = Mapping} = Mapped,

                       into_map(
                         sequence(
                           [kv(table_id, success(TableId)),
                            kv(flags, msmp_integer_fixed:decode(2)),
                            condition(
                              EventType == write_rows orelse
                              EventType == update_rows orelse
                              EventType == delete_rows,
                              kv(extra_row_info,
                                 length_encoded(
                                   map_result(
                                     msmp_integer_fixed:decode(2),
                                     fun
                                         (Length) ->
                                             Length - 2
                                     end)))),
                            kv(columns, msmp_integer_variable:decode()),
                            condition(
                              EventType == update_rows,
                              sequence(
                                [kv(bitmaps,
                                    into_tuple(
                                      pair(
                                        bitmap(length(ColTypes)),
                                        bitmap(length(ColTypes))))),

                                 kv(rows,
                                    condition(
                                      EventType == update_rows_compressed_v1,
                                      map_parser(
                                        uncompress(),
                                        many1(
                                          into_tuple(
                                            pair(
                                              row(Mapping),
                                              row(Mapping))))),

                                      %% uncompressed
                                      many1(
                                        into_tuple(
                                          pair(
                                            row(Mapping),
                                            row(Mapping))))))]),

                              sequence(
                                [kv(bitmap, bitmap(length(ColTypes))),
                                 kv(rows,
                                    condition(
                                      EventType == write_rows_compressed_v1,
                                      map_parser(
                                        uncompress(),
                                        many1(row(Mapping))),

                                      %% uncompressed
                                      many1(row(Mapping))))]))]))
               end))(Input)
    end;

event(Arg, #{event_type := EventType} = Header)
  when EventType == query;
       EventType == query_compressed ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{arg => Arg, header => Header}),

            (into_map(
               sequence(
                 [kv(slave_proxy_id, msmp_integer_fixed:decode(4)),
                  kv(execution_time, msmp_integer_fixed:decode(4)),
                  kv(schema_length, msmp_integer_fixed:decode(1)),
                  kv(error_code, msmp_integer_fixed:decode(2)),
                  condition(
                    version(Arg) >= 0,
                    sequence(
                      [kv(status_vars,
                          map_parser(
                            take(msmp_integer_fixed:decode(2)),
                            msmp_status_variable:decode())),
                       kv(schema, msmp_string_null_terminated:decode()),
                       condition(
                         EventType == query,
                         kv(sql, rest()),
                         kv(sql, uncompress()))]))])))(Input)
    end;

event(_Arg, #{event_type := intvar}) ->
    fun
        (Input) ->
            (into_map(
               sequence(
                 [scran_branch:alt(
                    [kv(type, scran_combinator:value(last_insert_id, tag(<<1>>))),
                     kv(type, scran_combinator:value(insert_id, tag(<<2>>)))]),
                  kv(value, msmp_integer_fixed:decode(8))])))(Input)
    end;

event(Arg, #{event_type := gtid_list} = Header) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{arg => Arg,
                         header => Header,
                         input => Input}),

            (scran_sequence:combined_with(
               into_map(
                 sequence(
                   [kv(count, scran_number:u(little, 28)),
                    kv(flags, scran_number:u(little, 4))])),
               fun
                   (#{count := Count} = Initial) ->
                       ?LOG_DEBUG(#{initial => Initial}),

                       into_map(
                         scran_sequence:sequence(
                           [kv(gtids,
                               scran_multi:count(
                                 Count,
                                 into_map(
                                   sequence(
                                     [kv(domain, msmp_integer_fixed:decode(4)),
                                      kv(server, msmp_integer_fixed:decode(4)),
                                      kv(sequence, msmp_integer_fixed:decode(8))]))))]))
               end))(Input)
    end;

event(Arg, #{event_type := binlog_checkpoint} = Header) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{arg => Arg,
                         header => Header,
                         input => Input}),
            (into_map(
               sequence(
                 [kv(filename, scran_bytes:take(scran_number:u32(little)))])))(Input)
    end;

event(Arg, #{event_type := gtid} = Header) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{arg => Arg,
                         header => Header,
                         input => Input}),
            (into_map(
               sequence(
                 [kv(sequence, msmp_integer_fixed:decode(8)),
                  kv(domain, msmp_integer_fixed:decode(4)),
                  kv(flags2, msmp_integer_fixed:decode(1)),
                  kv(data, scran_combinator:rest())])))(Input)
    end;

event(Arg, #{event_type := xid} = Header) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{arg => Arg,
                         header => Header,
                         input => Input}),
            (into_map(
               sequence(
                 [kv(xid, scran_bytes:take(8))])))(Input)
    end;

event(Arg, #{event_type := heartbeat_log} = Header) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{arg => Arg,
                         header => Header,
                         input => Input}),
            (into_map(
               sequence(
                 [kv(log_ident, rest())])))(Input)
    end;

event(Arg, #{event_type := stop} = Header) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{arg => Arg,
                         header => Header,
                         input => Input}),
            (into_map(
               sequence(
                 [ignore(scran_combinator:eof())])))(Input)
    end;

event(Arg, #{event_type := previous_gtids_log} = Header) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{arg => Arg,
                         header => Header,
                         input => Input}),
            (into_map(
               scran_sequence:sequence(
                 [kv(gtids, scran_multi:many1(scran_bytes:take(8)))])))(Input)
    end;

event(Arg, #{event_type := anonymous_gtid_log} = Header) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{arg => Arg,
                         header => Header,
                         input => Input}),
            (into_map(
               sequence(
                 [kv(data, rest())])))(Input)
    end;

event(Arg, #{event_type := heartbeat} = Header) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{arg => Arg,
                         header => Header,
                         input => Input}),
            (into_map(
               sequence(
                 [kv(unknown, scran_bytes:take(1)),
                  kv(log_ident, msmp_string_length_encoded:decode()),
                  kv(data, rest())])))(Input)
    end;

event(Arg, #{event_type := annotate_rows} = Header) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{arg => Arg,
                         header => Header,
                         input => Input}),
            (into_map(
               sequence(
                 [kv(annotation, rest())])))(Input)
    end;

event(Arg, Header) ->
    fun
        (Input) ->
            ?LOG_WARNING(#{arg => Arg,
                           input => Input,
                           header => Header}),
            (into_map(
               sequence(
                 [kv(body, rest())])))(Input)
    end.


field_metadata(ColTypes, Data) ->
    ?FUNCTION_NAME(ColTypes, Data, []).

field_metadata([string | ColTypes], <<Meta:16/little, Data/bytes>>, A) when Meta >= 256 ->
    <<RealType:8, Length:8>> = <<Meta:16/little>>,
    ?FUNCTION_NAME(
       ColTypes,
       Data,
       [#{field_type => msmp_field:lookup(RealType), length => Length} | A]);

field_metadata([string | ColTypes], <<Meta:16/little, Data/bytes>>, A) ->
    ?FUNCTION_NAME(
       ColTypes,
       Data,
       [#{length => Meta} | A]);

field_metadata([newdecimal | ColTypes], <<Precision:8, Scale:8, Data/bytes>>, A) ->
    ?FUNCTION_NAME(
       ColTypes,
       Data,
       [#{precision => Precision, scale => Scale} | A]);

field_metadata([ColType | ColTypes], <<Meta:16/little, Data/bytes>>, A)
  when ColType == var_string;
       ColType == varchar;
       ColType == bit ->
    ?FUNCTION_NAME(
       ColTypes,
       Data,
       [Meta | A]);

field_metadata([ColType | ColTypes], <<Meta:8/little, Data/bytes>>, A)
  when ColType == blob;
       ColType == double;
       ColType == geometry;
       ColType == json;
       ColType == time2;
       ColType == datetime2;
       ColType == timestamp2 ->
    ?FUNCTION_NAME(
       ColTypes,
       Data,
       [Meta | A]);

field_metadata([_ | ColTypes], Data, A) ->
    ?FUNCTION_NAME(ColTypes, Data, [undefined | A]);

field_metadata([], <<>>, A) ->
    maps:from_list(
      lists:zip(
        lists:seq(length(A), 1, -1),
        A));

field_metadata([], Remaining, A) ->
    ?LOG_DEBUG(#{remaining => Remaining}),
    maps:from_list(
      lists:zip(
        lists:seq(length(A), 1, -1),
        A)).


version(Arg) ->
    maps:get(version, Arg, 4).


header_size(Arg) ->
    maps:get(header_size,
             Arg,
             case version(Arg) of
                 Version when Version > 1 ->
                     19;
                 _Otherwise ->
                     13
             end).


rows(Mapping) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{mapping => Mapping, input => Input}),
            (many1(row(Mapping)))(Input)
    end.


row(#{coltypes := ColTypes,
      field_metadata := FieldMetadata,
      metadata := #{unsignedness := Unsignedness}} = Mapping) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{mapping => Mapping, input => Input}),

            (followed_with(
               into_map(
                 map_result(
                   map_parser(
                     into_bits(
                       bitmap(length(ColTypes)),
                       length(ColTypes)),
                     many1(into_boolean())),
                   fun
                       (Nulls) ->
                           lists:zip(
                             lists:seq(length(ColTypes), 1, -1),
                             Nulls)
                   end)),

               fun
                   (Nulls) ->
                       ?LOG_DEBUG(#{nulls => Nulls}),

                       into_tuple(
                         sequence(
                           lists:map(
                             fun
                                 ({ColNo, ColType}) ->
                                     case maps:get(ColNo, Nulls, false) of
                                         true ->
                                             success(null);

                                         false ->
                                             msmp_binlog_field:decode(
                                               ColType,
                                               maps:get(ColNo, Unsignedness, false),
                                               maps:get(ColNo, FieldMetadata, undefined))
                                     end
                             end,
                             lists:zip(
                               lists:seq(1, length(ColTypes)),
                               ColTypes))))
               end))(Input)
    end.


bitmap(N) ->
    msmp_integer_fixed:decode(bitmap_bytes(N)).


bitmap_bytes(N) ->
    msmp_binary:null_bitmap_bytes(N, 0).


uncompress() ->
    fun
        (Input) ->
            maybe
                {Compressed, Length} ?= (msmp_integer_fixed:decode(
                                           scran_combinator:map_result(
                                             msmp_integer_fixed:decode(1),
                                             fun
                                                 (Result) ->
                                                     Result band 16#07
                                             end)))(Input),

                case zlib:uncompress(Compressed) of
                    Uncompressed when byte_size(Uncompressed) == Length ->
                        {<<>>, Uncompressed};

                    _ ->
                        nomatch
                end
            end
    end.
