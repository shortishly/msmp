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


-module(msmp_server_status).


-export([decode/0]).


decode() ->
    fun
        (Input) ->
            Flags = names(),
            (scran_bytes:bitfield(
               Flags,
               scran_result:into_bits(
                 msmp_integer_fixed:decode(length(Flags) div 8),
                 length(Flags))))(Input)
    end.


names() ->
    lists:reverse(
      [status_in_trans,
       status_autocommit,
       more_results_exists,
       query_no_good_index_used,
       query_no_index_used,
       status_cursor_exists,
       status_last_row_sent,
       status_db_dropped,
       status_no_backslash_escapes,
       status_metadata_changed,
       query_was_slow,
       ps_out_params,
       status_in_trans_readonly,
       session_state_changed,
       reserved0,
       reserved1]).
