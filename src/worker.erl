%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% --------------------------------------------------------------------
-module(worker).   
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube_logger.hrl").
%% --------------------------------------------------------------------

% New final ?
 
-export([
	 add_path/2,
	 create_pod/4,
	 delete_pod/2,
	 load_start_application/6

	]).

%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
create_pod(HostId,NodeName,Dir,Args)->
    Result=case rpc:call(node(),slave,start,[HostId,NodeName,Args],5*1000) of
	       {badrpc,Reason}->
		   {error,[badrpc,Reason,HostId,NodeName,Dir,Args,
			   ?FUNCTION_NAME,?MODULE,?LINE]};
	       {error,Reason}->
		   {error,[Reason,HostId,NodeName,Dir,Args,
			   ?FUNCTION_NAME,?MODULE,?LINE]};
	       {ok,Node}->
		   rpc:call(node(),os,cmd,["rm -rf "++Dir],5*1000),
		   timer:sleep(100),
		   case rpc:call(node(),file,make_dir,[Dir],5*1000) of
		       {error,Reason}->
			   ?PrintLog(ticket,"Failed ",[Reason,HostId,Node,NodeName,Dir,Args,?FUNCTION_NAME,?MODULE,?LINE]),
			   {error,[Reason,HostId,Node,NodeName,?FUNCTION_NAME,?MODULE,?LINE]};
		       ok->
			   ?PrintLog(log,"Started ",[HostId,NodeName,Dir,Args,?FUNCTION_NAME,?MODULE,?LINE]),
			   {ok,Node}
		   end
	   end,
    Result.



%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
add_path(Node,EbinPath)->
    Result= case rpc:call(Node,code,add_patha,[EbinPath],5*1000) of
		{badrpc,Reason}->
		    {error,[badrpc,Reason,Node,EbinPath,
			    ?FUNCTION_NAME,?MODULE,?LINE]};
		{error, bad_directory}->
		    {error,[bad_directory,Node,EbinPath,
			    ?FUNCTION_NAME,?MODULE,?LINE]}; 
		true->
		    ok
	    end,
    Result.
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
load_start_application(Node,Dir,App,_AppVsn,GitPath,Env)->
    AppId=atom_to_list(App),  
    AppDir=filename:join(Dir,AppId),
    Ebin=filename:join(AppDir,"ebin"),

%    io:format("AppId,AppDir,Ebin ~p~n",[{AppId,AppDir,Ebin}]),
    Result=case rpc:call(Node,os,cmd,["rm -rf "++AppDir],5*1000) of
	       {badrpc,Reason}->
		   {error,[badrpc,Reason,Node,Dir,AppId,_AppVsn,GitPath,Env,
			   ?FUNCTION_NAME,?MODULE,?LINE]};	       
	       _->
		   case rpc:call(Node,os,cmd,["git clone "++GitPath++" "++AppDir],10*1000) of
		       {badrpc,Reason}->
			   {error,[badrpc,Reason,Node,Dir,AppId,_AppVsn,GitPath,Env,
				   ?FUNCTION_NAME,?MODULE,?LINE]};
		       _GitClone->
			   case rpc:call(Node,application,set_env,[[{App,Env}]],5*1000) of
			       {badrpc,Reason}->
				   {error,[badrpc,Reason,Node,Dir,AppId,_AppVsn,GitPath,Env,
					   ?FUNCTION_NAME,?MODULE,?LINE]};
			       _SetEnv->
				   case add_path(Node,Ebin) of
				       {error,Reason}->
					   {error,[Reason,Node,Dir,AppId,_AppVsn,GitPath,Env,
						   ?FUNCTION_NAME,?MODULE,?LINE]};
				       ok->
					   case rpc:call(Node,application,start,[App],5*1000) of
					       {badrpc,Reason}->
						   {error,[badrpc,Reason,Node,Dir,AppId,_AppVsn,GitPath,Env,
							   ?FUNCTION_NAME,?MODULE,?LINE]};
					       {error,Reason}->
						    {error,[Reason,Node,Dir,AppId,_AppVsn,GitPath,Env,
							   ?FUNCTION_NAME,?MODULE,?LINE]};
					       ok->
						   ok
					   end
				   end
			   end
		   end
	   end,
    Result.

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% -------------------------------------------------------------------
delete_pod(Node,Dir)->
    rpc:call(node(),os,cmd,["rm -rf "++Dir],5*1000),
    rpc:call(node(),slave,stop,[Node],5*1000).
  


