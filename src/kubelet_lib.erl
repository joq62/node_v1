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
	 create_pods/1,
	 delete_pods/1,
	 create_pod/1,
	 delete_pod/1
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

create_pod(Id)->
    ok=delete_pod(Id),
    {ok,HostId}=inet:gethostname(),
    {ok,ClusterId_X}=application:get_env(cluster_id),
    ClusterId=case is_atom(ClusterId_X) of
		  true->
		      atom_to_list(ClusterId_X);
		  false->
		      ClusterId_X
	      end,
    Cookie=atom_to_list(erlang:get_cookie()),
    NodeName=ClusterId++"_"++HostId++"_"++Id,
    Pod=list_to_atom(NodeName++"@"++HostId),
    Dir=Id++"."++ClusterId,
    
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
			   {ok,Pod}
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

