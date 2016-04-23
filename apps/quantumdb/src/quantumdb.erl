-module(quantumdb).
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([ping/0, get/1, delete/1, put/2, keys/1]).

-ignore_xref([ping/0, get/1, delete/1, put/2, keys/1]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
	quantumdb_metrics:core_ping(),
	send_command({<<"ping">>, term_to_binary(os:timestamp())}, ping).


get(Key) ->
	quantumdb_metrics:core_get(),
	send_command(Key, {get, Key}).

put(Key, Value) ->
	quantumdb_metrics:core_put(),
	send_command(Key, {put, Key, Value}).

 
delete(Key) ->
	quantumdb_metrics:core_delete(),
	send_command(Key, {delete, Key}).

keys(Bucket) ->
	quantumdb_metrics:core_keys(),
	Timeout = 5000,
	quantumdb_coverage_fsm:start({keys, Bucket}, Timeout).

% private functions
send_command(Key, Cmd) ->
    DocIdx = riak_core_util:chash_key(Key),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, quantumdb),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, Cmd, quantumdb_vnode_master).
