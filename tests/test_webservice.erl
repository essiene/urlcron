-module(test_webservice).
-include_lib("eunit/include/eunit.hrl").

helper_test() ->
    urlcron_scheduler:start(erlcfg:new("urlcron.conf")).

destroy_test() ->
    schedule_store:destroy(erlcfg:new("urlcron.conf")).
