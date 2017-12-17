{application, ioc2rpz,
 [
  {description, ""},
  {vsn, "0.9.0"},
  {modules, [
             ioc2rpz_app,
             ioc2rpz_sup,
             ioc2rpz,
             ioc2rpz_conn,
             ioc2rpz_fun,
             ioc2rpz_db,
             ioc2rpz_db_sup,
             ioc2rpz_udp
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { ioc2rpz_app, ["","./cfg/ioc2rpz.conf","./db"]}},
  {env, []}
 ]}.