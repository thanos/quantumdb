-module(quantumdb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    quantumdb_metrics:init(),
    init_http(),
    case quantumdb_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register([{vnode_module, quantumdb_vnode}]),
            ok = riak_core_node_watcher:service_up(quantumdb, self()),

            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.

% private functions

routes() ->
    [
        {"/ping", quantumdb_http_ping, []},
        {"/store/:bucket", quantumdb_http_store, []},
        {"/store/:bucket/:key", quantumdb_http_store, []}

    ].

init_http() ->
    DispatchRoutes = routes(),
    Dispatch = cowboy_router:compile([{'_', DispatchRoutes}]),

    CowboyOpts = [{env, [{dispatch, Dispatch}]}],
    ApiAcceptors = envd(http_acceptors, 100),
    ApiPort = envd(http_port, 8080),

    {ok, _} = cowboy:start_http(http, ApiAcceptors, [{port, ApiPort}],
                                CowboyOpts).

env(App, Par, Def) -> application:get_env(App, Par, Def).
envd(Par, Def) -> env(quantumdb, Par, Def).
