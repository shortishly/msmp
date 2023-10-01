<br>

<p align="center">
    <a href="https://shortishly.github.io/msmp/cover/">
      <img alt="Test Coverage" src="https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fshortishly.github.io%2Fmsmp%2Fcover%2Fcoverage.json&query=%24.total&suffix=%25&style=flat-square&label=Test%20Coverage&color=green">
    </a>
    <a href="https://shortishly.github.io/msmp/edoc/">
      <img alt="edoc" src="https://img.shields.io/badge/Documentation-edoc-green?style=flat-square">
    </a>
    <a href="https://erlang.org/">
      <img alt="Erlang/OTP 26+" src="https://img.shields.io/badge/Erlang%2FOTP-26%2B-green?style=flat-square">
    </a>
    <a href="https://www.apache.org/licenses/LICENSE-2.0">
      <img alt="Apache-2.0" src="https://img.shields.io/github/license/shortishly/msmp?style=flat-square">
    </a>
</p>

## What is msmp?

An Apache licensed MySQL/MariaDB pure protocol library using [scran
decoders][scran] and [narcs encoders][narcs]. An example client
[Erlang/OTP 26+][erlang-org] is [msc][msc] that includes replication
using this library.

## Protocol Support

The main motivation of this library is to provide good support for
binary replication in both MySQL and MariaDB.

### Replication Protocol

This section covers the binlog protocol used by
[MySQL][mysql-replication-protocol] and
[MariaDB][mariadb-replication-protocol] for binary replication.

The following binlog events are supported for binary replication via
[msmp\_binlog\_event](src/msmp_binlog_event.erl).

- annotate\_rows
- anonymous\_gtid\_log
- binlog\_checkpoint
- delete\_rows
- delete\_rows\_compressed\_v1
- delete\_rows\_v1
- format\_description
- gtid
- gtid\_list
- heartbeat
- heartbeat\_log
- intvar
- previous\_gtids\_log
- query
- query\_compressed
- rotate
- stop
- table\_map
- update\_rows
- update\_rows\_compressed\_v1
- update\_rows\_v1
- write\_rows
- write\_rows\_compressed\_v1
- write\_rows\_v1
- xid

The following binlog field types are supported when rows are being
updated, written or deleted:

- bit
- blob
- date
- datetime2
- double
- enum
- float
- int24
- JSON (the binary [JSON data type][mysql-json-data-type])
- long
- longlong
- newdecimal
- string
- time2
- timestamp2
- tiny
- varchar
- year

### Binary Protocol

There is also a binary protocol used to prepare, bind and execute
statements. Though the field types and encoding used are separate to
the ones used by replication.

- [msmp\_binary](src/msmp_binary.erl)
- [msmp\_binary\_resultset\_row](src/msmp_binary_resultset_row.erl)
- [msmp\_column\_count](src/msmp_column_count.erl)
- [msmp\_column\_definition](src/msmp_column_definition.erl)
- [msmp\_com\_stmt\_close](src/msmp_com_stmt_close.erl)
- [msmp\_com\_stmt\_execute](src/msmp_com_stmt_execute.erl)
- [msmp\_com\_stmt\_prepare](src/msmp_com_stmt_prepare.erl)
- [msmp\_com\_stmt\_prepare\_ok](src/msmp_com_stmt_prepare_ok.erl)
- [msmp\_com\_stmt\_reset](src/msmp_com_stmt_reset.erl)
- [msmp\_com\_stmt\_send\_long\_data](src/msmp_com_stmt_send_long_data.erl)

### Text Protocol

The text protocol is used for simple query:

- [msmp\_column\_count](src/msmp_column_count.erl)
- [msmp\_column\_definition](src/msmp_column_definition.erl)
- [msmp\_com\_query](src/msmp_com_query.erl)
- [msmp\_text](src/msmp_text.erl)
- [msmp\_text\_resultset\_row](src/msmp_text_resultset_row.erl)

### Connection Phase

The following are used in the connection phase of the protocol to
negotiate capabilities between the client and server, requesting SSL
(TLS), authentication and completing the handshake.

- [msmp\_auth\_more\_data](src/msmp_auth_more_data.erl)
- [msmp\_auth\_switch\_request](src/msmp_auth_switch_request.erl)
- [msmp\_capabilities](src/msmp_capabilities.erl)
- [msmp\_handshake](src/msmp_handshake.erl)
- [msmp\_handshake\_response](src/msmp_handshake_response.erl)
- [msmp\_ssl\_request](src/msmp_ssl_request.erl)

[erlang-org]: https://www.erlang.org
[mariadb-replication-protocol]: https://mariadb.com/kb/en/replication-protocol/
[msc]: https://github.com/shortishly/msc
[mysql-json-data-type]: https://dev.mysql.com/doc/refman/8.0/en/json.html
[mysql-replication-protocol]: https://dev.mysql.com/doc/dev/mysql-server/latest/page_protocol_replication.html
[narcs]: https://github.com/shortishly/narcs
[scran]: https://github.com/shortishly/scran
