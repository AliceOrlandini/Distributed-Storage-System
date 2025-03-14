-module(master_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    Reply = handle(Method, Req),
    {ok, Reply, Opts}.

handle(<<"POST">>,  Req) ->
    {ok, Headers, Req2} = cowboy_req:read_part(Req, #{length => infinit}),
    {ok, Data, _Req3} = cowboy_req:read_part_body(Req2),
    {file, Filename, _ContentType, _BitSize} = cow_multipart:form_data(Headers),
    Body = [{<<"filename">>, Filename}, {<<"base64_file">>, base64:encode(Data)}],
    file:write_file(Filename,Data),
    {ok, Body, Req};
handle(_, Req) ->
    cowboy_req:reply(404, #{}, <<"Not found">>, Req).
   

