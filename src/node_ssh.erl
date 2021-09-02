%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% --------------------------------------------------------------------
-module(node_ssh).   
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube_logger.hrl").
%% --------------------------------------------------------------------

% New final ?

-export([
	 create_node/4,
	 load_start_app/3

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

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
create_node(Alias,NodeName,Dir,Cookie)->
    ssh:start(),
    Result=case sd:call(etcd,db_host_info,read,[Alias],5*1000) of
	       []->
		   ?PrintLog(ticket,"eexists ",[Alias,NodeName,?FUNCTION_NAME,?MODULE,?LINE]),
		   {error,[eexists,Alias,?FUNCTION_NAME,?MODULE,?LINE]};
	       [{Alias,HostId,Ip,SshPort,UId,Pwd}]->
		   Node=list_to_atom(NodeName++"@"++HostId),
		  
		   true=erlang:set_cookie(Node,list_to_atom(Cookie)),
		   true=erlang:set_cookie(node(),list_to_atom(Cookie)),
		   ok=stop_node(Node,Dir),

		   ErlCmd="erl_call -s "++"-sname "++NodeName++" "++"-c "++Cookie,
		   SshCmd="nohup "++ErlCmd++" &",
		   ErlcCmdResult=rpc:call(node(),my_ssh,ssh_send,[Ip,SshPort,UId,Pwd,SshCmd,2*5000],3*5000),
		   case node_started(Node) of
		       false->
			   ?PrintLog(ticket,"Failed ",[Node,Alias,NodeName,ErlcCmdResult,?FUNCTION_NAME,?MODULE,?LINE]),
			   {error,['failed to start', Node,Alias,ErlcCmdResult,?FUNCTION_NAME,?MODULE,?LINE]};
		       true->
			   rpc:call(Node,os,cmd,["rm -rf "++Dir],5*1000),
			   case rpc:call(Node,file,make_dir,[Dir],5*1000) of
			       {error,Reason}->
				   ?PrintLog(ticket,"Failed ",[Reason,Node,Alias,NodeName,ErlcCmdResult,?FUNCTION_NAME,?MODULE,?LINE]),
				    {error,[Reason,Node,Alias,NodeName,ErlcCmdResult,?FUNCTION_NAME,?MODULE,?LINE]};
			       ok->
				   ?PrintLog(log,"Started ",[Node,Alias,NodeName,ErlcCmdResult,?FUNCTION_NAME,?MODULE,?LINE]),
				   {ok,Node}
			   end
		   end
	   end,
    Result.
    
stop_node(Node,Dir)->
    rpc:call(Node,os,cmd,["rm -rf "++Dir],5*1000),
    rpc:call(Node,init,stop,[],5*1000),		   
    Result=case node_stopped(Node) of
	       false->
		   ?PrintLog(ticket,"Failed to stop node ",[Node,?FUNCTION_NAME,?MODULE,?LINE]),
		   {error,["node not stopped",Node,?FUNCTION_NAME,?MODULE,?LINE]};
	       true->
		   ?PrintLog(log,"Stopped ",[Node,?FUNCTION_NAME,?MODULE,?LINE]),
		   ok
	   end,
    Result.

   
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

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------

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

