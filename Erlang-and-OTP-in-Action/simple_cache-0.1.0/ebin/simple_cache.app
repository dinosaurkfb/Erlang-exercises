
{application, simple_cache,
 [{description, "My simple cache for Erlang and OTP in action"},
  {vsn, "0.1.0"},
  {modules, [sc_app,
	     sc_sup,
	     sc_element_sup,
	     sc_event,
	     sc_event_logger
	     ]},
  {registered, [sc_sup, sc_element_sup]},
  {application, [kernel, stdlib]},
  {mod, {sc_app, []}}
]}.

