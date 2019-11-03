-module(http_handler).

-export([init/3, handle/2, terminate/3, init/2, doit/1]).
%example of talking to this handler:
%httpc:request(post, {"http://127.0.0.1:3010/", [], "application/octet-stream", "echo"}, [], []).
%curl -i -d '["test"]' http://localhost:3011
%curl -i -d echotxt http://localhost:3010
init(Req0, Opts) ->
    handle(Req0, Opts).	
handle(Req, State) ->
    {ok, Data, Req2} = cowboy_req:read_body(Req),
    {IP, _} = cowboy_req:peer(Req2),
    A = packer:unpack(Data),
    D = packer:pack(doit(A)),
    Headers = #{ <<"content-type">> => <<"application/octet-stream">>,
                 <<"Access-Control-Allow-Origin">> => <<"*">>},
    Req4 = cowboy_req:reply(200, Headers, D, Req2),
    {ok, Req4, State}.
init(_Type, Req, _Opts) -> {ok, Req, no_state}.
terminate(_Reason, _Req, _State) -> ok.

doit({read}) ->
    {ok, world:read()};
doit({take, X, Y, Z}) ->
    world:delete(X, Y, Z),
    {ok, 0};
doit({add, X, Y, Z, V}) ->
    world:add(X, Y, Z, V),
    {ok, 0}.



    
