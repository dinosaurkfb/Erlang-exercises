
{application, mytcp_rpc,
 [{description, "My RPC server for Erlang and OTP in action"},
  {vsn, "0.1.0"},
  {modules, [mytr_app,
	     mytr_sup,
	     mytr_server]},
  {registered, [mytr_sup]},
  {application, [kernel, stdlib]},
  {mod, {mytr_app, []}}
]}.

