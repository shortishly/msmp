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


-module(msmp_capabilities).


-export([decode/1]).
-export([encode/1]).
-export_type([combined/0]).
-export_type([lower/0]).
-export_type([upper/0]).


-type name() :: lower
              | upper
              | combined.

-type lower_name() :: reserved2
                    | reserved
                    | transactions
                    | ignore_sigpipe
                    | ssl
                    | interactive
                    | protocol_41
                    | ignore_space
                    | local_files
                    | odbc
                    | compress
                    | no_schema
                    | connect_with_db
                    | long_flag
                    | found_rows
                    | long_password.

-type lower() :: #{lower_name() := boolean()}.

-type upper_name() :: remember_options
                    | ssl_verify_cert
                    | capability_extension
                    | mfa
                    | query_attributes
                    | zstd_compression
                    | optional_resultset_metadata
                    | deprecate_eof
                    | session_track
                    | can_handle_expired_passwords
                    | plugin_auth_lenenc_client_data
                    | connect_attrs
                    | plugin_auth
                    | ps_multi_results
                    | multi_results
                    | multi_statements.

-type upper() :: #{upper_name() := boolean()}.

-type combined_name() :: lower_name()
                       | upper_name().

-type combined() :: #{combined_name() := boolean()}.

encode(Name) ->
    fun
        (Flags) ->
            Names = names(Name),
            Bits = length(Names),
            <<Encoded:Bits>> = (narcs_bits:into_bitfield(Flags))(Names),
            <<Encoded:Bits/little>>
    end.

decode(Name) ->
    fun
        (Input) ->
            Flags = names(Name),
            (scran_bytes:bitfield(
               Flags,
               scran_result:into_bits(
                 msmp_integer_fixed:decode(length(Flags) div 8),
                 length(Flags))))(Input)
    end.


-spec names(name()) -> [lower_name()] | [upper_name()] | [combined_name()].

names(combined) ->
    ?FUNCTION_NAME(upper) ++ ?FUNCTION_NAME(lower);

names(lower) ->
    [reserved2,
     reserved,
     transactions,
     ignore_sigpipe,
     ssl,
     interactive,
     protocol_41,
     ignore_space,
     local_files,
     odbc,
     compress,
     no_schema,
     connect_with_db,
     long_flag,
     found_rows,
     long_password];

names(upper) ->
    [remember_options,
     ssl_verify_cert,
     capability_extension,
     mfa,
     query_attributes,
     zstd_compression,
     optional_resultset_metadata,
     deprecate_eof,
     session_track,
     can_handle_expired_passwords,
     plugin_auth_lenenc_client_data,
     connect_attrs,
     plugin_auth,
     ps_multi_results,
     multi_results,
     multi_statements].
