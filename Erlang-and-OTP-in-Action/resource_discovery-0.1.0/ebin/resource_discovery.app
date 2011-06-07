{application, resource_discovery,
 [{description, "My resource discovery for Erlang and OTP in action"},
  {vsn, "0.1.0"},
  {modules, [rd_sup,
	     resource_discovery,
	     rd_watch
	     ]},
  {registered, [rd_sup]},
  {application, [kernel, stdlib]},
  {mod, {rd_app, []}}
]}.

