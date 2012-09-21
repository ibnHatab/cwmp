%%% File    : tr_soap_builder.erl
%%% Description :


-module(tr_soap_builder).

-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").

-include("tr69.hrl").
-include("proto.hrl").

-export([builder/1, build/1]).

-import(tr_soap_lib, [build_error/2, get_QName/2]).

%%%-----------------------------------------------------------------------------
%%%        SOAP Encoder
%%%-----------------------------------------------------------------------------


%% @doc Create an buildr/1 with the given options.
-spec builder([builder_option()]) -> function().
builder(Options) ->
    State = parse_builder_options(Options, #builder{}),
    fun (O) -> build_rpc_data(O, State) end.

%% @doc Build the given as SOAP to an rpc_data.
-spec build(#rpc_data{}) -> [export_element()].
build(Any) ->
    build_rpc_data(Any, #builder{}).


%%%-----------------------------------------------------------------------------
%% Internal API
%%%-----------------------------------------------------------------------------

parse_builder_options([], State) ->
    State;
parse_builder_options([{version, Version} | Rest], State) ->
    parse_builder_options(Rest, State#builder{version=Version});
parse_builder_options([{handler, Handler} | Rest], State) ->
    parse_builder_options(Rest, State#builder{handler=Handler});
parse_builder_options([{namespaces, Nss} | Rest], State) ->
    parse_builder_options(Rest, State#builder{ns=Nss}).

%%%-----------------------------------------------------------------------------
%%%        Build SOAP Envelope
%%%-----------------------------------------------------------------------------

%%  Build header
%%  Build body

%%%-----------------------------------------------------------------------------
%%%        Build SOAP Fault
%%%-----------------------------------------------------------------------------


%%%-----------------------------------------------------------------------------
%%%        Build RPC Message
%%%-----------------------------------------------------------------------------


-spec build_rpc_data(#rpc_data{}, #builder{}) ->  [export_element()].
build_rpc_data(#rpc_data{data=Data} = _D, State) ->
    ?DBG(Data),
    [build_Envelop(Data, State)].

-spec build_Envelop(#envelope{}, #builder{}) ->  export_element().
build_Envelop(#envelope{header=Header, body=Body} = _D, State) ->
    ElHeader = build_Header(Header, State),
    ElBody = build_Body(Body, State),
    Attribs = [{'xmlns:soap-env', 'http://schemas.xmlsoap.org/soap/envelope/'},
	       {'xmlns:soap-enc', 'http://schemas.xmlsoap.org/soap/encoding/'},
	       {'xmlns:xsd', 'http://www.w3.org/2001/XMLSchema'},
	       {'xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance'},
	       {'xmlns:cwmp', 'urn:dslforum-org:cwmp-1-0'}],
    {'soap-env:Envelop', Attribs, [ElHeader, ElBody]}.


-spec build_Header(#header{}, #builder{}) ->  export_element().
build_Header(_D, _State) ->
    'soap-env:Header'.

-spec build_Body(body_type(), #builder{}) ->  export_element().
build_Body(Body, State) ->
    {'soap-env:Body', [],
     [case Method of
	  #get_rpc_methods_response{} = Data -> build_GetRpcMethodsResponse(Data, State);
	  _ ->
	      build_error(Method, State)
      end || Method <- Body]}.


-spec build_GetRpcMethodsResponse(#get_rpc_methods_response{}, #builder{}) -> export_element().
build_GetRpcMethodsResponse(Data, State) ->   
    {'cwmp:GetRpcMethodsResponse', [], []
     %% [P || P <- [
     %% 		 build_MethodList (Data#get_rpc_methods_response.method_list, State)
     %% 		],
     %% 	   P /= null]
    }.



%'MethodList' ->


%%%-----------------------------------------------------------------------------
%%% Unitary tetsts
%%%-----------------------------------------------------------------------------
-define(RPC_DATA,
	{rpc_data,
	 {envelope,
	  {header,{id,true,"22_THOM_TR69_ID"},undefined,undefined},
	  [{get_rpc_methods_response,
	    ["GetRPCMethods","GetParameterNames",
	     "GetParameterValues","SetParameterValues","AddObject",
	     "DeleteObject","Download","Reboot","FactoryReset"]}]}}
       ).

-define(XML_NAMESPACE,
	{xmlNamespace,[],
	 [{"soapenc",'http://schemas.xmlsoap.org/soap/encoding/'},
	  {"soapenv",'http://schemas.xmlsoap.org/soap/envelope/'},
	  {"cwmp",'urn:dslforum-org:cwmp-1-0'}]}
       ).

-define(TAG_DATA,
	[{'Tag', [{attr, "Attributes"}], []}]).


main() ->
    Nss = tr_soap_lib:match_cwmp_ns_and_version(?XML_NAMESPACE),
    Builder = builder([{namespaces, Nss}]),
    Export = Builder(?RPC_DATA),
    ?DBG(Export),
    XML = xmerl:export_simple(Export, tr_soap_export, [{prolog,[]}]),
%%    ?DBG(unicode:characters_to_list(XML)),
    ?DBG(XML),
    ok.


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

build_rpc_data_test_no() ->
    Builder =  #builder{},
    XML = build_rpc_data(?RPC_DATA, Builder),
%    ?DBG(XML),
    ok.

export_test_no() ->
    Data =
	{'cwmp:bike',
	 [{year,"2003"},{color,"black"},{condition,"new"}],
	 [{'soap:name',
	   [{manufacturer,["Harley Davidsson"]},
	    {brandName,["XL1200C"]},
	    {additionalName,["Sportster"]}]},
	  {engine,
	   ["V-engine, 2-cylinders, 1200 cc"]},
	  {kind,["custom"]},
	  {drive,["belt"]}]},
    Prolog = ["<?xml version=\"1.0\" encoding=\"utf-8\" ?>\
 <!DOCTYPE motorcycles SYSTEM \"motorcycles.dtd\">\
"],
    RootEl = #xmlElement{content=[Data]},
    Data1 = [{'soap-env:Envelop'}],
    Data2 =
	{'soap-env:Envelop',
	 [],
	 []},
    Data3 =
	{envelope,[],
	 [
	  {header,[],
	   [{id,[{mustUnderstand,true}],
	     ["22_THOM_TR69_ID"]}]}
	 ]
	},

    XML = xmerl:export_simple([Data3], xmerl_xml, [{prolog,[]}]),
    ?DBG(unicode:characters_to_list(XML)).

tuple_test_no()->
    RootEl = #autonomous_transfer_complete{
      fault_struct = #transfer_complete_fault_struct{}
     },
    ?DBG(tuple_to_list(RootEl)).

main_test() ->
    main(),
    ok.


-endif.
