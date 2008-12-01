-module(test_webservice).
-include_lib("eunit/include/eunit.hrl").

helper_test() ->
    urlcron_scheduler:start(erlcfg:new("urlcron.conf")),
    urlcron_mochiweb:start(),
    application:start(inets),
    application:start(crypto).

destroy_test() ->
    application:stop(crypto),
    application:stop(inets),
    urlcron_mochiweb:stop(),
    schedule_store:destroy(erlcfg:new("urlcron.conf")).
