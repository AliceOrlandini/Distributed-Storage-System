-module(master_registration_handler).
-export([init/2]).

-behaviour(cowboy_handler).
-behaviour(application).

-record(user, {name, age}).

init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    Reply = handle(Method, Req),
    {ok, Reply, Opts}.

handle(<<"POST">>, Req) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    io:format("body ~p ~n", [Body]),
    
    case jiffy:decode(Body) of
        Json when is_map(Json) ->
        
            User = map_to_user(Json),
            io:format("user ~p ~n", User),
    
            Response = <<"Operazione completata">>,
            {ok, Req3} = cowboy_req:reply(200, #{}, Response, Req2),
            {ok, Req3, Req};
        _Error ->
            Error = <<"Body non valido">>,
            {ok, Req3} = cowboy_req:reply(400, #{}, Error, Req2),
            {ok, Req3, Req}
    end.

map_to_user(Map) ->
    #user{
        name = maps:get(<<"name">>, Map, <<"default">>),
        age = maps:get(<<"age">>, Map, 0)
    }.