%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(kubelet_lib).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

% New final ?

-export([
	 start_app/3,
	 start_app/4
	]).

%% External exports
-export([
	 

	]).


-export([

	]).



%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------


start_app(AppId,ClusterId,MonitorNode)->
    start_app(AppId,ClusterId,MonitorNode,[]).

start_app(AppId,ClusterId,MonitorNode,WantedHostAlias)->
    [PodInfo]=db_pod_spec:read(AppId),
    {_PodId,_PodVsn,_AppId,_AppVsn,_GitPath,_,HostAliasList}=PodInfo,
    AllNodeHostIdList=[{Node,rpc:call(Node,inet,gethostname,[],2*1000)}||{Node,_AppList}<-sd:all()], 
    KubeletNode=case HostAliasList of
		    []->
			case WantedHostAlias of
			    []->
				[{Node,_}|_]= AllNodeHostIdList,
				Node;
			    WantedHostAlias->
				[{_Alias,WantedHostId,_Ip,_SshPort,_UId,_Pwd}]=db_host_info:read(WantedHostAlias),	       
				[{Node,_HostId}]=[{Node,HostId}||{Node,{ok,HostId}}<-AllNodeHostIdList,
									WantedHostId==HostId],
				Node
			end;
		    HostAliasList->
			true=lists:keymember(WantedHostAlias,1,HostAliasList),
			[{_Alias,WantedHostId,_Ip,_SshPort,_UId,_Pwd}]=db_host_info:read(WantedHostAlias),	       
			[{Node,_HostId}]=[{Node,HostId}||{Node,{ok,HostId}}<-AllNodeHostIdList,
								WantedHostId==HostId],
			Node
		end,	
    pod:load_start(KubeletNode,ClusterId,MonitorNode,PodInfo).
     

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
