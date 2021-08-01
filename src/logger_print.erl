-module(logger_print).
-compile(export_all).

-include("kube_logger.hrl").





print_type(Type,FileNamen)->
    io:format("~n"),
    print_type(Type,20,FileNamen).

print_type(Type,N,FileName)->
    {ok,Info}=file:consult(FileName),
    RInfo=lists:reverse(Info),
    NInfo=lists:sublist(RInfo,N),
    [io:format("~s: ~w, , ~w, ~s, ~p, ~n",
	       [misc_fun:date_time(Date,Time),Node,Type,Msg,InfoList])||{Date,Time,Node,XType,Msg,InfoList}<-NInfo,
     XType==Type].
