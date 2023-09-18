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


-module(msmp_capabilities_tests).


-import(msmp_tests, [nm/1]).
-import(msmp_tests, [t/1]).
-include_lib("eunit/include/eunit.hrl").


lower_decode_test_() ->
    lists:map(
      t(msmp_capabilities:decode(lower)),
      [{#{interactive => true,
          ssl => true,
          compress => true,
          reserved => true,
          protocol_41 => true,
          transactions => true,
          reserved2 => true,
          ignore_sigpipe => true,
          ignore_space => true,
          local_files => true,
          odbc => true,
          no_schema => true,
          connect_with_db => true,
          long_flag => true,
          found_rows => true,
          long_password => true},
        <<255, 255>>}]).


lower_encode_test_() ->
    lists:map(
      t(msmp_capabilities:encode(lower)),
      [{<<255, 255>>,
        #{interactive => true,
          ssl => true,
          compress => true,
          reserved => true,
          protocol_41 => true,
          transactions => true,
          reserved2 => true,
          ignore_sigpipe => true,
          ignore_space => true,
          local_files => true,
          odbc => true,
          no_schema => true,
          connect_with_db => true,
          long_flag => true,
          found_rows => true,
          long_password => true}}]).


upper_decode_test_() ->
    lists:map(
      t(msmp_capabilities:decode(upper)),
      [{#{mfa => true,
          remember_options => true,
          ssl_verify_cert => true,
          capability_extension => false,
          query_attributes => true,
          zstd_compression => true,
          optional_resultset_metadata => true,
          deprecate_eof => true,
          session_track => true,
          can_handle_expired_passwords => true,
          plugin_auth_lenenc_client_data => true,
          connect_attrs => true,
          plugin_auth => true,
          ps_multi_results => true,
          multi_results => true,
          multi_statements => true},
        <<255, 223>>}]).


upper_encode_test_() ->
    lists:map(
      t(msmp_capabilities:encode(upper)),
      [{<<255, 223>>,
        #{mfa => true,
          remember_options => true,
          ssl_verify_cert => true,
          capability_extension => false,
          query_attributes => true,
          zstd_compression => true,
          optional_resultset_metadata => true,
          deprecate_eof => true,
          session_track => true,
          can_handle_expired_passwords => true,
          plugin_auth_lenenc_client_data => true,
          connect_attrs => true,
          plugin_auth => true,
          ps_multi_results => true,
          multi_results => true,
          multi_statements => true}}]).

combined_decode_test_() ->
    lists:map(
      t(msmp_capabilities:decode(combined)),
      [{#{interactive => true,
          ssl => false,
          compress => false,
          mfa => true,
          remember_options => false,
          ssl_verify_cert => false,
          capability_extension => false,
          query_attributes => true,
          zstd_compression => false,
          optional_resultset_metadata => false,
          deprecate_eof => true,
          session_track => true,
          can_handle_expired_passwords => true,
          plugin_auth_lenenc_client_data => true,
          connect_attrs => true,
          plugin_auth => true,
          ps_multi_results => true,
          multi_results => true,
          multi_statements => true,
          reserved2 => true,
          reserved => false,
          transactions => true,
          ignore_sigpipe => false,
          protocol_41 => true,
          ignore_space => false,
          local_files => true,
          odbc => false,
          no_schema => false,
          connect_with_db => false,
          long_flag => true,
          found_rows => false,
          long_password => true},
        <<133, 166, 255, 25>>},

       {#{interactive => false,
          ssl => false,
          compress => false,
          mfa => true,
          remember_options => true,
          ssl_verify_cert => false,
          capability_extension => false,
          query_attributes => true,
          zstd_compression => false,
          optional_resultset_metadata => false,
          deprecate_eof => true,
          session_track => true,
          can_handle_expired_passwords => false,
          plugin_auth_lenenc_client_data => true,
          connect_attrs => true,
          plugin_auth => true,
          ps_multi_results => false,
          multi_results => false,
          multi_statements => false,
          reserved2 => true,
          reserved => false,
          transactions => true,
          ignore_sigpipe => false,
          protocol_41 => true,
          ignore_space => false,
          local_files => true,
          odbc => false,
          no_schema => false,
          connect_with_db => false,
          long_flag => true,
          found_rows => false,
          long_password => true},
        <<133, 162, 184, 153>>}]).


encode_decode_test_() ->
    lists:map(
      fun
          (Test) ->
              {nm(Test),
               ?_assertEqual(
                  Test,
                  case (msmp_capabilities:decode(combined))
                      ((msmp_capabilities:encode(combined))
                         (Test)) of
                      {<<>>, Test} ->
                          Test
                  end)}
      end,
      [#{interactive => false,
         ssl => false,
          compress => false,
          mfa => true,
          zstd_compression => false,
          plugin_auth_lenenc_client_data => true,
          plugin_auth => true,
          connect_with_db => false,
          connect_attrs => true,
          remember_options => true,
          ssl_verify_cert => false,
          capability_extension => false,
          query_attributes => true,
          optional_resultset_metadata => false,
          deprecate_eof => true,
          session_track => true,
          can_handle_expired_passwords => false,
          ps_multi_results => false,
          multi_results => false,
          multi_statements => false,
          reserved2 => true,
          reserved => false,
          transactions => true,
          ignore_sigpipe => false,
          protocol_41 => true,
          ignore_space => false,
          local_files => true,
          odbc => false,
          no_schema => false,
          long_flag => true,
          found_rows => false,
          long_password => true}]).
