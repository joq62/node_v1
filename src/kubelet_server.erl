%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%%  NodeServer=      'ClusterId_HostId_0@HostId'
%%%  WorkerPod_1=     'ClusterId_HostId_1@HostId'
%%%  WorkerPod_N=     'ClusterId_HostId_N@HostId'
%%%  NodeServerDir=   0.ClusterId
%%%  WorkerPod_1_Dir= 1.ClusterId
%%%  N=20 
%%%  db_node:
%%%  [{Pod,Containers,Created},{Pod,Containers,Created}]
%%%  
%%%  env:ClusterId
%%% -------------------------------------------------------------------
-module(kubelet_server).  
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube_logger.hrl").
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state, {cluster_id,
		monitor_node,
		pods}).
	 


%% --------------------------------------------------------------------
%% Definitions 
%% --------------------------------------------------------------------
-define(KubeleId,0).
-define(NumWorkerPods,10).
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

-export([]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================


%%-----------------------------------------------------------------------

%%----------------------------------------------------------------------


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: 
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%
%% --------------------------------------------------------------------
init([]) ->
    {ok,_}=kube_logger:start(),
    ssh:start(),
    ClusterId=sd:call(etcd,db_cluster_info,cluster,[],5*1000),
    MonitorNode=sd:call(etcd,db_cluster_info,monitor,[],5*1000),

    ?PrintLog(log,"Successful starting of server",[?MODULE]),
    ok=rpc:call(node(),kubelet_lib,scratch,[],10*1000),
  %  CreateResult=pod:create_pods(?NumWorkerPods),
  %  ActivePods=[Pod||{ok,Pod}<-CreateResult],
    {ok, #state{cluster_id=ClusterId,
		monitor_node=MonitorNode,
		pods=[]}}.
    
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (aterminate/2 is called)
%% --------------------------------------------------------------------

handle_call({load_start_app,Node,AppId,Dir},_From,State) ->
    Reply=rpc:call(node(),kubelet_lib,load_start_app,[Node,AppId,Dir],20*1000),
    {reply,Reply,State};

handle_call({stop_unload_app,Node,AppId,Dir},_From,State) ->
    Reply=rpc:call(node(),kubelet_lib,stop_unload_app,[Node,AppId,Dir],10*1000),
    {reply,Reply,State};

handle_call({create_vm_ssh,Alias,NodeName,Dir,Cookie},_From,State) ->
    Reply=rpc:call(node(),node_ssh,create_node,[Alias,NodeName,Dir,Cookie],30*1000),
    {reply,Reply,State};

handle_call({create_vm,NodeName,Dir},_From,State) ->
    Reply=rpc:call(node(),kubelet_lib,create_vm,[NodeName,Dir],10*1000),
    {reply,Reply,State};

handle_call({delete_vm,NodeName,Dir},_From,State) ->
    Reply=rpc:call(node(),kubelet_lib,delete_vm,[NodeName,Dir],10*1000),
    {reply,Reply,State};

handle_call({create_pod,Id},_From,State) ->
    Reply=rpc:call(node(),kubelet_lib,create_pod,[Id],10*1000),
    {reply,Reply,State};

handle_call({scratch_pod,Pod},_From,State) ->
    Reply=rpc:call(node(),kubelet_lib,scratch_pod,[Pod],10*1000),
    {reply,Reply,State};

handle_call({load_start,Container},_From,State) ->
    Reply=rpc:call(node(),container,load_start,[Container,State#state.pods],10*1000),
    {reply,Reply,State};

handle_call({stop_unload,Pod,Container},_From,State) ->
    Reply=rpc:call(node(),container,stop_unload,[Pod,Container],10*1000),
    {reply,Reply,State};


handle_call({ping},_From,State) ->
    Reply={pong,node(),?MODULE},
    {reply, Reply, State};

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% -------------------------------------------------------------------
handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_info(Info, State) ->
    io:format("unmatched match info ~p~n",[{?MODULE,?LINE,Info}]),
    {noreply, State}.


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
