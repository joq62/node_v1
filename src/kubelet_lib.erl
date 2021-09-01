%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% --------------------------------------------------------------------
-module(kubelet_lib).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube_logger.hrl").
%% --------------------------------------------------------------------

% New final ?

-export([
	 create_vm/2,
	 delete_vm/2,
	 scratch/0,

	 load_start_app/3,
	 load_start_app/6,
	 stop_unload_app/3,
	 
	 load_start/2,
	 stop_unload/2,
	 
	 create_pods/1,
	 delete_pods/1,
	 create_pod/1,
	 delete_pod/1
	]).

%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
load_start_app(Node,AppId,Dir)->
    Result=case db_pod_spec:containers(AppId) of
	       {error,Reason}->
		   {error,Reason};
	       [{AppId,AppVsn,GitPath,Env}]->
		   kubelet_lib:load_start_app(Node,Dir,AppId,AppVsn,GitPath,Env)
	   end,
    Result.
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------

load_start_app(Node,Dir,AppId,AppVsn,GitPath,Env)->
    AppDir=filename:join(Dir,AppId),
    Ebin=filename:join(AppDir,"ebin"),
    App=list_to_atom(AppId),
    os:cmd("rm -rf "++AppDir),
    _R=rpc:call(Node,os,cmd,["git clone "++GitPath++" "++AppDir],10*1000),
    _SetEnv=rpc:call(Node,application,set_env,[[{App,Env}]],5*1000),
   % io:format("SetEnv ~p~n",[SetEnv]),
    _AddCode=rpc:call(Node,code,add_patha,[Ebin],5*1000),
 %   io:format("AddCode ~p~n",[AddCode]),    
  %  io:format("~p~n",[R]),
    ok=rpc:call(Node,application,start,[App],5*1000),
    ok.
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
stop_unload_app(Node,AppId,Dir)->
    AppDir=filename:join(Dir,AppId),
    Ebin=filename:join(AppDir,"ebin"),
    App=list_to_atom(AppId),
    ok=rpc:call(Node,application,stop,[App],5*1000),
    ok=rpc:call(Node,application,unload,[App],5*1000),
    true=rpc:call(Node,code,del_path,[Ebin],5*1000),
    rpc:call(Node,os,cmd,["rm -rf "++AppDir],5*1000),
    ok.
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
scratch()->
    {ok,FileNames}=file:list_dir("."),
    FilesToScratch=[FileName||FileName<-FileNames,
				   ".deployment"==filename:extension(FileName)],
    [os:cmd("rm -rf "++FileToScratch)||FileToScratch<-FilesToScratch],
    HostName=net_adm:localhost(),
    [rpc:call(list_to_atom(FileToScratch++"@"++HostName),init,stop,[],5*1000)||FileToScratch<-FilesToScratch],

    ok.
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
create_vm(NodeName,Dir)->
    delete_vm(NodeName,Dir),
    Cookie=atom_to_list(erlang:get_cookie()),
    HostName=net_adm:localhost(),
    Args="-setcookie "++Cookie,
    Result=case slave:start(HostName, NodeName,Args) of
	       {error, Reason}->
		   {error,[Reason,?FUNCTION_NAME,?MODULE,?LINE]};
	       {ok,Node}->
		   case file:make_dir(Dir) of
		       {error, Reason}->
			   {error,[Reason,?FUNCTION_NAME,?MODULE,?LINE]};
		       ok->
			   {ok,Node}
		   end
	   end,
    Result.
    
    

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
delete_vm(Node,Dir)->
    os:cmd("rm -rf "++Dir),
    slave:stop(Node),
    ok.

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------


load_start({AppId,AppVsn,GitPath,AppEnv},Pods)->
    NumPods=lists:flatlength(Pods),
    N=rand:uniform(NumPods),
    WorkerPod=lists:nth(N,Pods),
    Dir=db_kubelet:dir(WorkerPod),
    Result=case load(AppId,AppVsn,GitPath,AppEnv,WorkerPod,Dir) of
	       {error,Reason}->
		   {error,Reason};
	       ok ->
		   case start(AppId,WorkerPod) of
		       {error,Reason}->
			   {error,Reason};
		       ok->
			   {atomic,ok}=db_kubelet:add_container(WorkerPod,{AppId,AppVsn,GitPath,AppEnv}),
			   {ok,{AppId,AppVsn,GitPath,AppEnv},WorkerPod}
		   end
	   end,
    Result.
    
load(AppId,_AppVsn,GitPath,AppEnv,Pod,Dir)->
    Result = case rpc:call(Pod,application,which_applications,[],5*1000) of
		 {badrpc,Reason}->
		     {error,[badrpc,Reason,?FUNCTION_NAME,?MODULE,?LINE]};
		 LoadedApps->
		     case lists:keymember(list_to_atom(AppId),1,LoadedApps) of
			 true->
			     ?PrintLog(log,'Already loaded',[AppId,Pod,?FUNCTION_NAME,?MODULE,?LINE]),
			     {error,['Already loaded',AppId,Pod]};
			 false ->
			     AppDir=filename:join(Dir,AppId),
			     AppEbin=filename:join(AppDir,"ebin"),
			     App=list_to_atom(AppId),
			     rpc:call(Pod,os,cmd,["rm -rf "++AppId],25*1000),
			     _GitResult=rpc:call(Pod,os,cmd,["git clone "++GitPath],25*1000),
				%	   ?PrintLog(log,"GitResult",[PodNode,GitPath,GitResult,?FUNCTION_NAME,?MODULE,?LINE]),
			     _MVResult=rpc:call(Pod,os,cmd,["mv "++AppId++" "++AppDir],25*1000),
				%	   ?PrintLog(log,"MVResult",[AppId,AppDir,MVResult,?FUNCTION_NAME,?MODULE,?LINE]),
			     true=rpc:call(Pod,code,add_patha,[AppEbin],22*1000),
			     ok=rpc:call(Pod,application,set_env,[[{App,AppEnv}]]),		       
			     ok
		     end
	     end,
    Result.

start(AppId,Pod)->
    App=list_to_atom(AppId),
    ?PrintLog(debug,"App,Pod",[App,Pod,?FUNCTION_NAME,?MODULE,?LINE]),
    Result=case rpc:call(Pod,application,start,[App],2*60*1000) of
	       ok->
		   ok;
	       {error,{already_started}}->
		   ok;
	       {Error,Reason}->
		   {Error,[Reason,application,Pod,start,App,?FUNCTION_NAME,?MODULE,?LINE]}
	   end,
    Result.

    
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
stop_unload(Pod,{AppId,AppVsn,GitPath,AppEnv})->
    Dir=db_kubelet:dir(Pod),
    AppDir=filename:join(Dir,AppId),
    App=list_to_atom(AppId),
    rpc:call(Pod,application,stop,[App],5*1000),
    rpc:call(Pod,application,unload,[App],5*1000),
    rpc:call(Pod,os,cmd,["rm -rf "++AppDir],3*1000),
    {atomic,ok}=db_kubelet:delete_container(Pod,{AppId,AppVsn,GitPath,AppEnv}),
    ok.
    
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
create_pods(Num)->
    create_pods(Num,[]).
create_pods(0,StartResult)->
    StartResult;
create_pods(N,Acc) ->
    R=create_pod(integer_to_list(N)),
    create_pods(N-1,[R|Acc]).

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
delete_pods(Num)->
    delete_pods(Num,[]).
delete_pods(0,StartResult)->
    StartResult;
delete_pods(N,Acc) ->
    R=delete_pod(integer_to_list(N)),
    delete_pods(N-1,[R|Acc]).
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------

create_pod(PodId)->
    ok=delete_pod(PodId),
    {ok,HostId}=inet:gethostname(),
    {ok,ClusterId_X}=application:get_env(cluster_id),
    ClusterId=case is_atom(ClusterId_X) of
		  true->
		      atom_to_list(ClusterId_X);
		  false->
		      ClusterId_X
	      end,
    Cookie=atom_to_list(erlang:get_cookie()),
    NodeName=ClusterId++"_"++HostId++"_"++PodId,
    Pod=list_to_atom(NodeName++"@"++HostId),
    Dir=PodId++"."++ClusterId,
    
    %ok=delete_pod(Pod,Dir),

    ErlCallArgs="-c "++Cookie++" "++"-sname "++NodeName,
    ErlCmd="erl_call -s "++ErlCallArgs, 
    ErlCmdResult=os:cmd(ErlCmd),
    Result=case node_started(Pod) of
	       false->
		   {error,[not_started,Pod,ErlCmdResult,?FUNCTION_NAME,?MODULE,?LINE]};
	       true ->
		  
		   case file:make_dir(Dir) of
		       {error,Reason}->
			   {error,[Reason,Pod,Dir,?FUNCTION_NAME,?MODULE,?LINE]};
		       ok->
			   case db_kubelet:member(PodId,HostId,ClusterId) of
				       true->
					   {error,['already exists',PodId,HostId,ClusterId,?FUNCTION_NAME,?MODULE,?LINE]};
				       false->
					   {atomic,ok}=db_kubelet:create(PodId,HostId,ClusterId,Pod,Dir,node(),Cookie,[]),
					   {ok,Pod}
				   end
		   end
	   end,
    Result.
delete_pod(Id)->
    {ok,HostId}=inet:gethostname(),
    {ok,ClusterId_X}=application:get_env(cluster_id),
    ClusterId=case is_atom(ClusterId_X) of
		  true->
		      atom_to_list(ClusterId_X);
		  false->
		      ClusterId_X
	      end,
    NodeName=ClusterId++"_"++HostId++"_"++Id,
    Pod=list_to_atom(NodeName++"@"++HostId),
    Dir=Id++"."++ClusterId,
    delete_pod(Pod,Dir).

delete_pod(Pod,Dir)->
    rpc:call(Pod,os,cmd,["rm -rf "++Dir],5*1000),
    rpc:call(Pod,init,stop,[],5*1000),		   
    Result=case node_stopped(Pod) of
	       false->
		   {error,["node not stopped",Pod,?FUNCTION_NAME,?MODULE,?LINE]};
	       true->
		   db_kubelet:delete(Pod),
		   ok
	   end,
    Result.

    
    
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
	      
node_started(Node)->
    check_started(100,Node,50,false).
    
check_started(_N,_Vm,_SleepTime,true)->
   true;
check_started(0,_Vm,_SleepTime,Result)->
    Result;
check_started(N,Vm,SleepTime,_Result)->
 %   io:format("net_Adm ~p~n",[net_adm:ping(Vm)]),
    NewResult= case net_adm:ping(Vm) of
	%case rpc:call(node(),net_adm,ping,[Vm],1000) of
		  pong->
		     true;
		  pang->
		       timer:sleep(SleepTime),
		       false;
		   {badrpc,_}->
		       timer:sleep(SleepTime),
		       false
	      end,
    check_started(N-1,Vm,SleepTime,NewResult).

node_stopped(Node)->
    check_stopped(100,Node,50,false).
    
check_stopped(_N,_Vm,_SleepTime,true)->
   true;
check_stopped(0,_Vm,_SleepTime,Result)->
    Result;
check_stopped(N,Vm,SleepTime,_Result)->
 %   io:format("net_Adm ~p~n",[net_adm:ping(Vm)]),
    NewResult= case net_adm:ping(Vm) of
	%case rpc:call(node(),net_adm,ping,[Vm],1000) of
		  pang->
		     true;
		  pong->
		       timer:sleep(SleepTime),
		       false;
		   {badrpc,_}->
		       true
	       end,
    check_stopped(N-1,Vm,SleepTime,NewResult).

