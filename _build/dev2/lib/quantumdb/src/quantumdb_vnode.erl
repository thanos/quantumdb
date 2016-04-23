-module(quantumdb_vnode).
-behaviour(riak_core_vnode).

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

-ignore_xref([
             start_vnode/1
             ]).

-record(state, {partition, table_id, table_name}).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    TableName = list_to_atom("quantumdb_" ++ integer_to_list(Partition)),
    TableId = ets:new(TableName, 
                        [set, public, named_table,
                        {write_concurrency, false},
                        {read_concurrency, true}]),

    {ok, #state{partition=Partition, table_id=TableId,
            table_name=TableName}}.

%% Sample command: respond to a ping
handle_command(ping, _Sender, State) ->
    {reply, {pong, State#state.partition}, State};

handle_command({put, Key, Value}, _Sender,
    State=#state{table_name=TableName, partition=Partition}) ->
        ets:insert(TableName, {Key, Value}),
        {reply, {ok, Partition}, State};

handle_command({get, Key}, _Sender,
    State=#state{table_name=TableName, partition=Partition}) ->
        case ets:lookup(TableName, Key) of
            [] -> 
                {reply, {not_found, Partition, Key}, State};
            [Value] ->
                {reply, {found, Partition, {Key, Value}}, State}
        end;

handle_command({delete, Key}, _Sender,
               State=#state{table_name=TableName, partition=Partition}) ->
    case ets:lookup(TableName, Key) of
        [] ->
            {reply, {not_found, Partition, Key}, State};
        [Value] ->
            true = ets:delete(TableName, Key),
            {reply, {found, Partition, {Key, Value}}, State}
    end;

handle_command(Message, _Sender, State) ->
    lager:warning("unhandled_command ~p", [Message]),
    {noreply, State}.

handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

handle_coverage({keys, Bucket}, _KeySpaces, {_, RefId, _},
    State=#state{table_name=TableName}) ->
    Keys0 = ets:match(TableName, {{Bucket, '$1'}, '_'}),
    Keys = lists:map(fun first/1, Keys0),
    {reply, {RefId, Keys}, State};

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
      {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.


% private functions
first([V|_]) -> V.

