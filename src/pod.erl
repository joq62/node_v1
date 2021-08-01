%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(pod).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube_logger.hrl").
%%---------------------------------------------------------------------
%% Records for test
%%

%% --------------------------------------------------------------------
-compile(export_all).


%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
load_start(Node,ClusterId,MonitorNode,{_PodId,_PodVsn,AppId,_AppVsn,GitPath,AppEnv,_AppHosts})->
    % clone application 
    PathApp=filename:join([ClusterId,AppId]),
    rpc:call(Node,os,cmd,["rm -rf "++PathApp],5*1000),
    ok=rpc:call(Node,file,make_dir,[PathApp],5*1000),
    _CloneResult=rpc:call(Node,os,cmd,["git clone "++GitPath++" "++PathApp],10*1000),
  %  ?PrintLog(debug,"CloneResult",[?MODULE,?LINE,CloneResult]),
      %Start application
    Ebin=filename:join(PathApp,"ebin"),
    rpc:call(Node,code,add_patha,[Ebin],5*1000),
    App=list_to_atom(AppId),
    NewAppEnv=lists:append([{kubelet,[{monitor_node,MonitorNode},{cluster_id,ClusterId}]}],AppEnv),
    %?PrintLog(debug,"NewAppEnv",[?MODULE,?LINE,NewAppEnv]),
    SetEnvResult=rpc:call(Node,application,set_env,[NewAppEnv],5*1000),
    %?PrintLog(debug,"SetEnvResult",[?MODULE,?LINE,SetEnvResult]),
    case rpc:call(Node,application,start,[App],20*1000) of
	ok->
	    ok;
	Reason->
	    ?PrintLog(ticket,"error",[Reason,Node,ClusterId,App,?FUNCTION_NAME,?MODULE,?LINE])
    end,
	
    % Check if started
    Apps=rpc:call(Node,application,which_applications,[],5*1000),
    true=lists:keymember(App,1,Apps),
    ok.

stop_unload(Node,ClusterId,{_PodId,_PodVsn,AppId,_AppVsn,_GitPath,_AppEnv,_AppHosts})->
    App=list_to_atom(AppId),
    rpc:call(Node,application,stop,[App],5*1000),
    rpc:call(Node,application,unload,[App],5*1000),
    PathApp=filename:join([ClusterId,AppId]),
    rpc:call(Node,os,cmd,["rm -rf "++PathApp],10*1000),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
load_start(ClusterId,{_PodId,_PodVsn,AppId,_AppVsn,GitPath,AppEnv,_AppHosts})->
    % clone application 
    PathApp=filename:join([ClusterId,AppId]),
    os:cmd("rm -rf "++PathApp),
    ok=file:make_dir(PathApp),
    os:cmd("git clone "++GitPath++" "++PathApp),
      %Start application
    Ebin=filename:join(PathApp,"ebin"),
    code:add_patha(Ebin),
    App=list_to_atom(AppId),
    NewAppEnv=lists:append([{cluster_id,ClusterId}],AppEnv),
    application:set_env(NewAppEnv),
    ok=application:start(App),
    % Check if started
    Apps=application:which_applications(),
    true=lists:keymember(App,1,Apps),
    ok.

stop_unload(ClusterId,{_PodId,_PodVsn,AppId,_AppVsn,_GitPath,_AppEnv,_AppHosts})->
    App=list_to_atom(AppId),
    application:stop(App),
    application:unload(App),
    PathApp=filename:join([ClusterId,AppId]),
    os:cmd("rm -rf "++PathApp),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
load(RemoteNode,Dir,{_PodId,_PodVsn,AppId,_AppVsn,GitPath,AppEnv,_AppHosts})->
    
    IsDir=rpc:call(RemoteNode,filelib,is_dir,[Dir],5*1000),
    case IsDir of
	true->
	    ok;
	false->
	    ok=rpc:call(RemoteNode,file,make_dir,[Dir],5*1000)
    end,
    % clone application 
    PathApp=filename:join([Dir,AppId]),
    ok=rpc:call(RemoteNode,file,make_dir,[PathApp],5*1000),
    rpc:call(RemoteNode,os,cmd,["git clone "++GitPath++" "++PathApp],10*1000),
      %Start application
    Ebin=filename:join(PathApp,"ebin"),
    rpc:call(RemoteNode,code,add_patha,[Ebin],5*1000),
    App=list_to_atom(AppId),
    case AppEnv of
	[]->
	    ok;
	AppEnv ->
	    rpc:call(RemoteNode,application,set_env,[AppEnv])
    end,

    ok=rpc:call(RemoteNode,application,start,[App]),
    % Check if started
    Apps=rpc:call(RemoteNode,application,which_applications,[]),
    true=lists:keymember(App,1,Apps),
    ok.
    
unload(RemoteNode,Dir,{_PodId,_PodVsn,AppId,_AppVsn,_GitPath,_AppEnv,_AppHosts})->
    App=list_to_atom(AppId),
    rpc:call(RemoteNode,application,stop,[App]),
    rpc:call(RemoteNode,application,unload,[App]),
    PathApp=filename:join([Dir,AppId]),
    rpc:call(RemoteNode,os,cmd,["rm -rf "++PathApp]),
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

delete(Node,Dir)->
    slave:stop(Node),
    os:cmd("rm -rf "++Dir),
    ok.
    
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

create({PodId,_PodVsn,AppId,_AppVsn,GitPath,AppEnv,_AppHosts}) ->
    % Unique PodId
    Unique=integer_to_list(erlang:system_time(millisecond)),
    NodeId=Unique++"_"++PodId,
    % Create unique dir
    ok=file:make_dir(NodeId),
    % clone application
    PathApp=filename:join([NodeId,AppId]),
    ok=file:make_dir(PathApp),
    os:cmd("git clone "++GitPath++" "++PathApp),
    %Start Slave
    {ok,Host}=inet:gethostname(),
    Cookie=atom_to_list(erlang:get_cookie()),
    Ebin=filename:join([PathApp,"ebin"]),
    SlaveArgs="-pa "++Ebin++" "++"-setcookie "++Cookie,
    {ok,Node}=slave:start(Host,NodeId,SlaveArgs),
    %Start application
    App=list_to_atom(AppId),
    case AppEnv of
	[]->
	    ok;
	AppEnv ->
	    rpc:call(Node,application,set_env,[AppEnv])
    end,
    ok=rpc:call(Node,application,start,[App]),
    % Check if started
    AppsSlave=rpc:call(Node,application,which_applications,[]),
    true=lists:keymember(App,1,AppsSlave),
    {ok,Node,NodeId}.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

delete(KubeletNode,Node,Dir)->
    rpc:call(KubeletNode,slave,stop,[Node],5*1000),
    rpc:call(KubeletNode,os,cmd,["rm -rf "++Dir],5*1000),
    ok.

create(KubeletNode,{PodId,_PodVsn,AppId,_AppVsn,GitPath,AppEnv,_AppHosts}) ->
    % Unique PodId
    Unique=integer_to_list(erlang:system_time(millisecond)),
    NodeId=Unique++"_"++PodId,
    % Create unique dir
    ok=rpc:call(KubeletNode,file,make_dir,[NodeId],5*1000),
    % clone application 
    PathApp=filename:join([NodeId,AppId]),
    ok=rpc:call(KubeletNode,file,make_dir,[PathApp],5*1000),
    rpc:call(KubeletNode,os,cmd,["git clone "++GitPath++" "++PathApp],10*1000),
    %Start Slave
    {ok,Host}=rpc:call(KubeletNode,inet,gethostname,[],5*1000),
    io:format("Host ~p~n",[Host]),
    Cookie=atom_to_list(rpc:call(KubeletNode,erlang,get_cookie,[],5*1000)),
    io:format("Cookie ~p~n",[Cookie]),
    Ebin=filename:join([PathApp,"ebin"]),
    SlaveArgs="-pa "++Ebin++" "++"-setcookie "++Cookie,
    {ok,Node}=rpc:call(KubeletNode,slave,start,[Host,NodeId,SlaveArgs],5*1000),
    %Start application
    App=list_to_atom(AppId),
    case AppEnv of
	[]->
	    ok;
	AppEnv ->
	    rpc:call(Node,application,set_env,[AppEnv])
    end,
    ok=rpc:call(Node,application,start,[App]),
    % Check if started
    AppsSlave=rpc:call(Node,application,which_applications,[]),
    true=lists:keymember(App,1,AppsSlave),
    {ok,Node,NodeId}.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
node(Name)->
    {ok,HostId}=net:gethostname(),
    list_to_atom(Name++"@"++HostId).
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
vmid_hostid(Node)->
    NodeStr=atom_to_list(Node),
    [VmId,HostId]=string:lexemes(NodeStr,"@"),
    {VmId,HostId}.
