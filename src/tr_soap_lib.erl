%%% File    : tr_soap_lib.erl
%%% Description : SOAP import/export routines


-module(tr_soap_lib).

-include_lib("xmerl/include/xmerl.hrl").

-include("proto.hrl").


-export([import/1, export/1]).


export(SOAP_Envelop) ->
    {ok, RPC_Data} = parseSoapMessage(SOAP_Envelop).

%-spec import(#xmlElement{}) -> {ok, rpcData ()}.

-spec import(#xmlElement{}) -> {ok, #rpcData{}} | {error, any()}.
import(_CWMP_Method) ->
    {ok, ok}.



parseSoapMessage(SOAP) ->
    ok.

    %% Child = getFirstChild (SOAP),
    %% case getNodeName (SOAP) of
    %% 	"soap:Header" -> parseHeader ()
    %% end.
