%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(kubelet_test).   
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include_lib("eunit/include/eunit.hrl").
%% --------------------------------------------------------------------

%% External exports
-export([start/0]). 


%% ====================================================================
%% External functions
%% ====================================================================


%% --------------------------------------------------------------------
%% Function:tes cases
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
start()->
    io:format("~p~n",[{"Start setup",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=setup(),
    io:format("~p~n",[{"Stop setup",?MODULE,?FUNCTION_NAME,?LINE}]),

    io:format("~p~n",[{"Start worker()",?MODULE,?FUNCTION_NAME,?LINE}]),
 %   ok=worker(),
    io:format("~p~n",[{"Stop worker()",?MODULE,?FUNCTION_NAME,?LINE}]),

%    io:format("~p~n",[{"Start app()",?MODULE,?FUNCTION_NAME,?LINE}]),
%    ok=app(),
%    io:format("~p~n",[{"Stop app()",?MODULE,?FUNCTION_NAME,?LINE}]),

  %  io:format("~p~n",[{"Start node_ssh()",?MODULE,?FUNCTION_NAME,?LINE}]),
  %  ok=node_ssh(),
  %  io:format("~p~n",[{"Stop node_ssh)",?MODULE,?FUNCTION_NAME,?LINE}]),

  %  io:format("~p~n",[{"Start pass_0()",?MODULE,?FUNCTION_NAME,?LINE}]),
   % ok=pass_0(),
   % io:format("~p~n",[{"Stop pass_0()",?MODULE,?FUNCTION_NAME,?LINE}]),

 %   io:format("~p~n",[{"Start pass_1()",?MODULE,?FUNCTION_NAME,?LINE}]),
 %   ok=pass_1(),
 %   io:format("~p~n",[{"Stop pass_1()",?MODULE,?FUNCTION_NAME,?LINE}]),

%    io:format("~p~n",[{"Start pass_2()",?MODULE,?FUNCTION_NAME,?LINE}]),
%    ok=pass_2(),
%    io:format("~p~n",[{"Stop pass_2()",?MODULE,?FUNCTION_NAME,?LINE}]),

%    io:format("~p~n",[{"Start pass_3()",?MODULE,?FUNCTION_NAME,?LINE}]),
%    ok=pass_3(),
%    io:format("~p~n",[{"Stop pass_3()",?MODULE,?FUNCTION_NAME,?LINE}]),

  %  io:format("~p~n",[{"Start pass_4()",?MODULE,?FUNCTION_NAME,?LINE}]),
  %  ok=pass_4(),
  %  io:format("~p~n",[{"Stop pass_4()",?MODULE,?FUNCTION_NAME,?LINE}]),

  %  io:format("~p~n",[{"Start pass_5()",?MODULE,?FUNCTION_NAME,?LINE}]),
  %  ok=pass_5(),
  %  io:format("~p~n",[{"Stop pass_5()",?MODULE,?FUNCTION_NAME,?LINE}]),
 
    
   
      %% End application tests
    io:format("~p~n",[{"Start cleanup",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=cleanup(),
    io:format("~p~n",[{"Stop cleaup",?MODULE,?FUNCTION_NAME,?LINE}]),
   
    io:format("------>"++atom_to_list(?MODULE)++" ENDED SUCCESSFUL ---------"),
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
% Kubelet inherit etcd and support ?
% Share that path to slaves 
%
%
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
worker()->
    HostId=net_adm:localhost(),
    Name1="n1",
    Dir1=Name1++".deployment",
    Cookie=atom_to_list(erlang:get_cookie()),
    Args1="-setcookie "++Cookie,
   
    {ok,Node1}=worker:create_pod(HostId,Name1,Dir1,Args1),
    D=date(),
    D=rpc:call(Node1,erlang,date,[],3*1000),
    Pod1="mymath",
    
    % Add path to support and other common functions
    ok=worker:add_path(Node1,"ebin"),
    
    {"n1","joq62-X550CA"}=rpc:call(Node1,misc_node,vmid_hostid,[Node1],1*1000),
    
    
    
    AppList=db_pod_spec:application_list(Pod1),
    _Replicas=db_pod_spec:replicas(Pod1),
    _HostList=db_pod_spec:host_list(Pod1),
    [worker:load_start_application(Node1,Dir1,App,AppVsn,GitPath,Env)||{App,AppVsn,GitPath,Env}<-AppList],
    42=rpc:call(Node1,mymath,add,[20,22],3*1000),
    timer:sleep(1000),
    
    ok=worker:delete_pod(Node1,Dir1),
    {badrpc,_}=rpc:call(Node1,erlang,date,[],3*1000),
    

    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_1()->
    
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_0()->
    {ok,ClusterId_X}=application:get_env(cluster_id),
    ClusterId=case is_atom(ClusterId_X) of
		  true->
		      atom_to_list(ClusterId_X);
		  false->
		      ClusterId_X
	      end,
    PodsList=[Pods||{_DeploymentId,_Vsn,Pods,_ClusterId}<-db_deployment_spec:key_cluster_id(ClusterId)],
    ContainersList=[db_pod_spec:containers(PodId)||[{PodId,_Vsn,_Num}]<-PodsList],
    ContainersToStart=lists:append(ContainersList),
    StartList=[kubelet:load_start(Container)||Container<-ContainersToStart],
    
    io:format(" 1. db_kubelet:read_all() ~p~n",[{db_kubelet:read_all(),?MODULE,?LINE}]),
    
    
    
    StopList=[kubelet:stop_unload(Pod,Container)||{ok,Container,Pod}<-StartList],
    io:format(" 2. db_kubelet:read_all() ~p~n",[{db_kubelet:read_all(),?MODULE,?LINE}]),
    
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_2()->
    Id1=integer_to_list(1),
    Id2=integer_to_list(2),
    Id3=integer_to_list(3),
    {ok,Pod1}=kubelet_lib:create_pod(Id1),
    {ok,Pod2}=kubelet_lib:create_pod(Id2),
    {ok,Pod3}=kubelet_lib:create_pod(Id3),
    D=date(),
    D=rpc:call(Pod1,erlang,date,[],2*1000),
    D=rpc:call(Pod2,erlang,date,[],2*1000),
    D=rpc:call(Pod3,erlang,date,[],2*1000),

    timer:sleep(1000),
    ok=kubelet_lib:delete_pod(Id1),
    timer:sleep(1000),
    ok=kubelet_lib:delete_pod(Id2),
    timer:sleep(1000),
    ok=kubelet_lib:delete_pod(Id3),
    
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_3()->
  
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_4()->
  
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_5()->
  
    
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

setup()->
 %   ok=etcd:start_init_mnesia(),
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------    

cleanup()->
  
%    application:stop(kubelet),
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
