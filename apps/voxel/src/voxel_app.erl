%%%-------------------------------------------------------------------
%% @doc voxel public API
%% @end
%%%-------------------------------------------------------------------

-module(voxel_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    inets:start(),
    start_http(),
    R = voxel_sup:start_link(),
    world:new(),
    spawn(fun() ->
                  world_cron()
          end),
    R.

stop(_State) ->
    ok.

%% internal functions
start_http() ->
    Dispatch =
        cowboy_router:compile(
          [{'_', [
		  %{"/:file", file_handler, []}%,
		  {"/", http_handler, []},
		  {"/[...]", file_handler, []}
		 ]}]),
    %{ok, Port} = application:get_env(amoveo_mining_pool, port),
    {ok, _} = cowboy:start_clear(http,
				 [{ip, {0,0,0,0}}, {port, 8095}],
				 #{env => #{dispatch => Dispatch}}),
    ok.

world_cron() ->
    timer:sleep(3000),
    spawn(fun() ->
                  world:compress()
          end),
    world_cron().
    

    
