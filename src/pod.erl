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
-export([load_start/4,
	 stop_unload/3
	]).


%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
load_start(Node,ClusterId,MonitorNode,{_PodId,_PodVsn,AppId,_AppVsn,GitPath,AppEnv,_AppHosts})->
 %   ?PrintLog(debug,"1  pod:load_start ",[Node,ClusterId,MonitorNode,{_PodId,_PodVsn,AppId,_AppVsn,GitPath,AppEnv,_AppHosts},?FUNCTION_NAME,?MODULE,?LINE]),
			 % clone application 
    PathApp=filename:join([ClusterId,AppId]),
    rpc:call(Node,os,cmd,["rm -rf "++PathApp],5*1000),
    ok=rpc:call(Node,file,make_dir,[PathApp],5*1000),
    _CloneResult=rpc:call(Node,os,cmd,["git clone "++GitPath++" "++PathApp],10*1000),
      %Start application
    Ebin=filename:join(PathApp,"ebin"),
    rpc:call(Node,code,add_patha,[Ebin],5*1000),
    App=list_to_atom(AppId),
    NewAppEnv=lists:append([{kubelet,[{monitor_node,MonitorNode},{cluster_id,ClusterId}]}],AppEnv),
    _SetEnvResult=rpc:call(Node,application,set_env,[NewAppEnv],5*1000),
    case rpc:call(Node,application,start,[App],20*1000) of
	ok->
	    AgainCookie=erlang:get_cookie(),
	    NodeCookie=rpc:call(Node,erlang,get_cookie,[],5*1000),
	    case AgainCookie==NodeCookie of
		true->
		    ?PrintLog(debug,"cookie ok ",[NodeCookie, AgainCookie,Node,?FUNCTION_NAME,?MODULE,?LINE]),
		    ok;
		false->
		    rpc:call(Node,erlang,set_cookie,[Node,AgainCookie],5*1000),
		    ?PrintLog(ticket,"Needed to re-set cookie ",[NodeCookie, AgainCookie,Node,?FUNCTION_NAME,?MODULE,?LINE]),
		    ok	
	    end;
	Reason->
	    ?PrintLog(ticket,"error",[Reason,Node,ClusterId,App,?FUNCTION_NAME,?MODULE,?LINE])
    end,
	
    % Check if started
    Apps=rpc:call(Node,application,which_applications,[],5*1000),
    case lists:keymember(App,1,Apps) of
	true->
	    {ok,App,Node};
	false->
	    ?PrintLog(ticket,"App not started  ",[App,Node,Apps,?FUNCTION_NAME,?MODULE,?LINE]),
	    {error,[ticket,"App not started  ",[App,Node,Apps,?FUNCTION_NAME,?MODULE,?LINE]]}
	 
    end.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
stop_unload(Node,ClusterId,{_PodId,_PodVsn,AppId,_AppVsn,_GitPath,_AppEnv,_AppHosts})->
    App=list_to_atom(AppId),
    rpc:call(Node,application,stop,[App],5*1000),
    rpc:call(Node,application,unload,[App],5*1000),
    PathApp=filename:join([ClusterId,AppId]),
    rpc:call(Node,os,cmd,["rm -rf "++PathApp],10*1000),
    ok.

