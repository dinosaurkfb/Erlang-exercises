
{application, simple_cache,
 [{description, "My simple cache for Erlang and OTP in action"},
  {vsn, "0.3.0"},
  {modules, [simple_cache,
  	     sc_app,
	     sc_sup,
	     sc_element_sup,
	     sc_store,
	     sc_element,
	     sc_event,
	     sc_event_logger
	     ]},
  {registered, [sc_sup, sc_element_sup]},
  {application, [kernel, sasl, stdlib, mnesia, resource_discovery]},
  {mod, {sc_app, []}}
]}.

