-module(webservice).
-export([
       get/2
    ]).


get("/", Req) ->
    get("", Req);
get("", Req) ->
    QueryString = Req:parse_qs(),
    Req:ok({"text/plain", io_lib:format("~p", [QueryString])});
get(_Path, Req) ->
    Req:not_found().
