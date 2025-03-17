-module(slave_handler).

-export([init/2]).

init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    Reply = handle(Method, Req),
    {ok, Reply, Opts}.

handle(<<"GET">>, Req) ->
    io:format("GET request~n"),
    % qui ci sarà nell'header un token che contiene il nome del file
    % e l'expiration time
    cowboy_req:reply(200, #{}, <<"ok">>, Req);
handle(_, Req) ->
    cowboy_req:reply(404, #{}, <<"Not found">>, Req).
    
