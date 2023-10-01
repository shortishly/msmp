{application, 'msmp', [
	{description, "MySQL/MariaDB Message Protocol"},
	{vsn, ""},
	{id, "3a83822-dirty"},
	{modules, ['msmp','msmp_auth_more_data','msmp_auth_switch_request','msmp_binary','msmp_binary_resultset_row','msmp_binlog','msmp_binlog_dump','msmp_binlog_event','msmp_binlog_field','msmp_binlog_network_stream','msmp_capabilities','msmp_codec','msmp_column_count','msmp_column_definition','msmp_column_definition_flags','msmp_com_query','msmp_com_stmt_close','msmp_com_stmt_execute','msmp_com_stmt_prepare','msmp_com_stmt_prepare_ok','msmp_com_stmt_reset','msmp_com_stmt_send_long_data','msmp_cursor','msmp_debug','msmp_decimal','msmp_enum','msmp_field','msmp_field_optional_metadata','msmp_handshake','msmp_handshake_response','msmp_integer_fixed','msmp_integer_variable','msmp_jsonb','msmp_narcs','msmp_null_bitmap','msmp_packet_eof','msmp_packet_error','msmp_packet_ok','msmp_register_replica','msmp_server_status','msmp_ssl_request','msmp_status_variable','msmp_string_fixed','msmp_string_length_encoded','msmp_string_null_terminated','msmp_string_rest_of_packet','msmp_text','msmp_text_resultset_row','msmp_uri']},
	{registered, []},
	{applications, [kernel,stdlib,narcs,phrase,scran]},
	{optional_applications, []},
	{env, []}
]}.