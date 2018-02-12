{application, 'my_little_proxy', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['my_little_proxy_app','my_little_proxy_sup']},
	{registered, [my_little_proxy_sup]},
	{applications, [kernel,stdlib]},
	{mod, {my_little_proxy_app, []}},
	{env, []}
]}.