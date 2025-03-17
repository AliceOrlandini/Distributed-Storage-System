-module(slave_handler).

-export([init/2]).

init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    Reply = handle(Method, Req,Opts),
    {ok, Reply, Opts}.

handle(<<"GET">>, Req, State) ->
    io:format("GET request, State: ~p~n", [State]),
    FileName = maps:get(file_name, State, <<"non_definito">>),
    io:format("FileName: ~p~n", [FileName]),
    % qui ci sarà nell'header un token che contiene il nome del file
    % e l'expiration time
    cowboy_req:reply(200, #{}, <<"ok">>, Req);
handle(_, Req, _) ->
    cowboy_req:reply(404, #{}, <<"Not found">>, Req).
    
