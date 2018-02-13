{application, 'my_little_proxy', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['mlp_server','mlp_server_sup','mlp_worker','mlp_worker_sup','my_little_proxy_app','my_little_proxy_sup']},
	{registered, [my_little_proxy_sup]},
	{applications, [kernel,stdlib]},
	{mod, {my_little_proxy_app, []}},
	{env, []}
]}.