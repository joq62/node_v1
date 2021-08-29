%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(controller_lib).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------

% New final ?

-export([
	 new_pod/1,

	 desired_state/1,
	 status_all_pods/0,
	 strive_desired_state/1

	]).



%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
new_pod(ClusterId)->
    AllDepSpecs=db_deployment_spec:key_cluster_id(ClusterId),
    DepSpecPodsInfoList=[{DepId,Pods}||{DepId,_,Pods,_}<-AllDepSpecs],

    %% WantedPods
    WantedPods=lists:append([get_pod_spec(DepId,Pods)||{DepId,Pods}<-DepSpecPodsInfoList]),
 %   io:format("WantedPods ~p~n",[{WantedPods,?MODULE,?LINE}]),   
 
    AvailableHosts=get_available_hosts(ClusterId),
 %   io:format("AvailableHosts ~p~n",[{AvailableHosts,?MODULE,?LINE}]),

    DesiredState=lists:append(desired_state(WantedPods,AvailableHosts)),
    io:format("DesiredState ~p~n",[{DesiredState,?MODULE,?LINE}]),
    
  %  StartResult=create_pod(DesiredState,ClusterId,[]),
    
    StartResult=new_pod(DesiredState,ClusterId),
    UpdateResult=update_dbase(StartResult,ClusterId,[]),
    
    UpdateResult.
update_dbase([],_ClusterId,UpdateResult)->
    UpdateResult;
update_dbase([StartResult|T],ClusterId,Acc)->
%    [{result,{ok,'1630002870775525_lgh@c0',[{ok,"mymath"}]}},
%      {pod_info,[{info,"dep_1","mymath","1.0.0",1},
%		 {containers,[{"mymath","1.0.0","https://github.com/joq62/mymath.git",[]}]},
%		 {host,{"c0_lgh","c0"}}]},
%      {node_name,"1630002870775525_lgh"},
%      {dir,"1630002870775525.lgh"}]
    
    case lists:keyfind(result,1,StartResult) of
	{error,Reason}->
	    NewAcc=[{error,Reason}|Acc];
	{result,{ok,Pod,AppIdList}}->
	    {pod_info,[{info,DeploymentSpec,_PodName,_Vsn,_Num},
		       {containers,Containers},{host,{Alias,HostId}}]}=lists:keyfind(pod_info,1,StartResult),
	    {dir,Dir}=lists:keyfind(dir,1,StartResult),
	    {atomic,ok}=db_deployment:create(DeploymentSpec,todo_add_deployment_vsn,Pod,Dir,HostId,ClusterId,status_glurk),
	    case rpc:call(Pod,erlang,put,[deployment_spec,DeploymentSpec],5*1000) of
		{badrpc,Reason}->
		    NewAcc=[{error,Reason}|Acc];
		_->
		    NewAcc=[{ok,DeploymentSpec,HostId,Pod,Containers}|Acc]
	    end
    end,
    update_dbase(T,ClusterId,NewAcc).

new_pod(DesiredState,ClusterId)->
    F1=fun create_pod/2,
    F2=fun create_result/3,
    Vector=[{PodInfo,ClusterId}||PodInfo<-DesiredState],
    Result=mapreduce:start(F1,F2,[],Vector),
    
    Result.


create_pod(Pid,{PodInfo,ClusterName})->
    UniqueId=integer_to_list(erlang:system_time(microsecond)), 
    NodeName=UniqueId++"_"++ClusterName,
    Dir=UniqueId++"."++ClusterName,
    Cookie=db_cluster_spec:cookie(ClusterName),
  %  io:format("PodInfo ~p~n",[{PodInfo,?MODULE,?LINE}]),
    R=pod_controller:new(PodInfo,NodeName,Dir,Cookie),
    Pid!{create_pod,[{result,R},{pod_info,PodInfo},{node_name,NodeName},{dir,Dir}]}.

create_result(create_pod,Vals,[])->
   % io:format("Vals ~p~n",[{Vals,?MODULE,?LINE}]),
    create_result(Vals,[]).

create_result([],StartResult)->
    StartResult;
create_result([Result|T],Acc) ->
    
    io:format("Result ~p~n",[{Result,?MODULE,?LINE}]),
    create_result(T,[Result|Acc]).


desired_state(WantedPods,AvailableHosts)->
    desired_state(WantedPods,AvailableHosts,[]).
    
desired_state([],_AvailableHosts,DesiredState)->
    DesiredState;
desired_state([Pods|T],AvailableHosts,Acc)->
    {info,DepId,Name,Vsn,Num}=lists:keyfind(info,1,Pods),
    {containers,Containers}=lists:keyfind(containers,1,Pods),
    {wanted_hosts,WantedHosts}=lists:keyfind(wanted_hosts,1,Pods),
    Result=case filter_hosts(WantedHosts,AvailableHosts) of
	       {error,Reason}->
		   {error,Reason};
	       Candidates->
		   Len=lists:flatlength(Candidates),
		   case Len<Num of
		       true->
			   {error,['not enough of hosts',Candidates,Num,?MODULE,?FUNCTION_NAME,?LINE]};
		       false->
			   create_deployment(DepId,Name,Vsn,Containers,Candidates,Num,[])
		   end
	   end,
    desired_state(T,AvailableHosts,[Result|Acc]).
   
create_deployment(_DepId,_Name,_Vsn,_Containers,_Candidates,0,DesiredDeployment)-> 
    DesiredDeployment;
create_deployment(DepId,Name,Vsn,Containers,Candidates,Num,Acc)->
    Info=[{info,DepId,Name,Vsn,Num},{containers,Containers},{host,lists:nth(Num,Candidates)}],
    create_deployment(DepId,Name,Vsn,Containers,Candidates,Num-1,[Info|Acc]).

    
filter_hosts(_,[])->
    {error,[eexists,hosts,?MODULE,?FUNCTION_NAME,?LINE]};
filter_hosts([],AvailableHosts)->
    AvailableHosts;
filter_hosts(WantedHosts,AvailableHosts) ->
    [HostInfo||HostInfo<-WantedHosts,
	       lists:member(HostInfo,AvailableHosts)].    

get_available_hosts(ClusterId)->
    AllClusterHosts=db_cluster_spec:hosts(ClusterId),
  %  io:format("AllHosts ~p~n",[{AllHosts,?MODULE,?LINE}]),
    AllRunningHosts=running_hosts(),   
   % io:format("AllRunningHosts ~p~n",[{AllRunningHosts,?MODULE,?LINE}]),

    AvailableHosts=[{Alias,HostId}||{Alias,HostId}<-AllClusterHosts,
				    lists:member({Alias,HostId},AllRunningHosts)],
    %io:format("AvailableHosts ~p~n",[{AvailableHosts,?MODULE,?LINE}]),
    AvailableHosts.

running_hosts()->
    {RunningHosts,_}=iaas:status_all_hosts(),
    Running=[{Alias,HostId}||{running,Alias,HostId,_Ip,_Port}<-RunningHosts],
    Running.
    

p_info(L)->
    io:format("info ~p~n",[{[lists:keyfind(info,1,X)||X<-L],?MODULE,?LINE}]).
    
get_pod_spec(DepId,Pods)->
    get_pod_spec(Pods,DepId,[]).
get_pod_spec([],_DepId,ExtractedList)->
    ExtractedList;
get_pod_spec([{Name,Vsn,Num}|T],DepId,Acc)->
    Containers=db_pod_spec:containers(Name),
    WantedHosts=db_pod_spec:wanted_hosts(Name),
    get_pod_spec(T,DepId,[[{info,DepId,Name,Vsn,Num},{containers,Containers},{wanted_hosts,WantedHosts}]|Acc]).

    
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
desired_state(ClusterId)->
    AllDepSpecs=db_deployment_spec:key_cluster_id(ClusterId),
    DepSpecPodsInfoList=[{DepId,Pods}||{DepId,_,Pods,_}<-AllDepSpecs],
    
    DepSpecPodsInfoList.
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
status_all_pods()->
    {ok,ClusterIdAtom}=application:get_env(controller,cluster_id),
    ClusterId=atom_to_list(ClusterIdAtom),
    Pods2Deploy=db_deployment_spec:pods(ClusterId),
 %   io:format("Pods2Deploy ~p~n",[{Pods2Deploy,?FUNCTION_NAME,?MODULE,?LINE}]),

  %  io:format("db_pod_spec ~p~n",[{db_pod_spec:read_all(),?FUNCTION_NAME,?MODULE,?LINE}]),

    AppInfo=[{db_pod_spec:app_id(PodId),DeploymentId,PodId,HostId}||{DeploymentId,PodId,HostId}<-Pods2Deploy],
 %   io:format("PodIds ~p~n",[{AppInfo,?FUNCTION_NAME,?MODULE,?LINE}]),
    
    %% 
    SdGet=[{sd:get(list_to_atom(AppId)),AppId,DeploymentId,PodId,HostId}||{AppId,DeploymentId,PodId,HostId}<-AppInfo],
  %  io:format("SdGet ~p~n",[{SdGet,?FUNCTION_NAME,?MODULE,?LINE}]),
    
    %%
    {Running,Missing}=check_status(SdGet,[],[]),
  %  io:format("Running ~p~n",[{Running,?FUNCTION_NAME,?MODULE,?LINE}]),
  %  io:format("Missing ~p~n",[{Missing,?FUNCTION_NAME,?MODULE,?LINE}]),
    {ok,Running,Missing}.

    %%

check_status([],Running,Missing)->
    {Running,Missing};
check_status([{[],AppId,DeploymentId,PodId,HostId}|T],R,M)->
    check_status(T,R,[{AppId,DeploymentId,PodId,HostId}|M]);
check_status([{Nodes,AppId,DeploymentId,PodId,HostId}|T],R,M)->
    VmIdsHostIds=[misc_node:vmid_hostid(Node)||Node<-Nodes],
    case lists:keymember(HostId,2,VmIdsHostIds) of
	true->
	    NewR=[{AppId,DeploymentId,PodId,HostId}|R],
	    NewM=M;
	false->
	    NewR=R,
	    NewM=[{AppId,DeploymentId,PodId,HostId}|M]
    end,
    check_status(T,NewR,NewM).

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
strive_desired_state({[],[]})->
    {[],[]};
strive_desired_state({_RunningPods,MissingPods})->
    _StarResult=load_start(MissingPods,[]),
   % io:format("StarResult ~p~n",[{StarResult,?FUNCTION_NAME,?MODULE,?LINE}]),
    {ok,Running,Missing}=status_all_pods(),
    {Running,Missing}.

load_start([],StarResult)->
    StarResult;
load_start([{_AppId,_DeploymentId,PodId,HostId}|T],Acc)->
    R=case pod:create(HostId) of
	  {ok,Ref}->
	      pod:load_start(PodId,Ref);
	  Err ->
	      {error,[Err,?FUNCTION_NAME,?MODULE,?LINE]}
      end,
    load_start(T,[R|Acc]).
