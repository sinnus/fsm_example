{application, fsm_application,
 [{description, "fsm_application"},
  {vsn, "0.01"},
  {modules, [
    fsm_application,
    fsm_sup,
    reloader
  ]},
  {registered, []},
  {mod, {fsm_application, []}},
  {env, []},
  {applications, [kernel, stdlib]}]}.
