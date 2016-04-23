-module(quantumdb_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    VMaster = { quantumdb_vnode_master,
                  {riak_core_vnode_master, start_link, [quantumdb_vnode]},
                  permanent, 5000, worker, [riak_core_vnode_master]},

	CoverageFSMs = {quantumdb_coverage_fsm_sup,
		{quantumdb_coverage_fsm_sup, start_link, []},
		permanent, infinity, supervisor, [quantumdb_coverage_fsm_sup]},

    { ok,
        { {one_for_one, 5, 10},
          [VMaster, CoverageFSMs]}}.
