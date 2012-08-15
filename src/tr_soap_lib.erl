%%% File    : tr_soap_lib.erl
%%% Description : SOAP import/export routines


-module(tr_soap_lib).

-include_lib("xmerl/include/xmerl.hrl").

-include("tr69.hrl").
-include("proto.hrl").


-export([read_xml/1, write_xml/1]).

-export([get_QName/2,
	 get_local_name/2,
	 xmlText/1,
	 xmlElement/1
	]).



-export([encoder/1, encode/1]).
%% @doc parse XML document from string
-spec read_xml(string()) -> Result when
      Result :: #xmlElement{} | {error, any()}.
read_xml(Str) ->
    case catch xmerl_scan:string(Str) of
	{'EXIT',Reason} ->
	    {error,Reason};
	{error,Reason}	->
	    {error,Reason};
	{ParsResult, _} -> ParsResult
	end.

-spec write_xml(Doc) -> Result when
      Doc :: #xmlElement{},
      Result :: list().
write_xml(Doc) ->
    Bin = xmerl:export_simple([Doc], xmerl_xml),
    unicode:characters_to_list(Bin).

%%%-----------------------------------------------------------------------------
%% Internal API
%%%-----------------------------------------------------------------------------

%% Usefull filtering predicates
-compile({nowarn_unused_function, xmlText/1}).
xmlText(_=#xmlText{}) -> true;
xmlText(_) -> false.

-compile({nowarn_unused_function, xmlElement/1}).
xmlElement(_=#xmlElement{}) -> true;
xmlElement(_) -> false.

-compile({nowarn_unused_function, get_QName/2}).
get_QName(Name, Ns) when is_atom(Name) ->
    get_QName(atom_to_list(Name), Ns);
get_QName(Name, Ns) when is_list(Name) ->
    list_to_atom(Ns ++ ":" ++ Name).

local_name(Name) when is_atom(Name) ->
    local_name(atom_to_list(Name));
local_name(Name) when is_list(Name) ->
    case lists:splitwith(fun($:) -> false;(_)->true end,Name) of
	{Ns,":"++LocalName} -> {Ns, list_to_atom(LocalName)};
	_ ->
	    {"", list_to_atom(Name)}
    end.

get_local_name(Name, Ns) when is_atom(Name)->
    case local_name(Name) of
        {Ns, LocalName} -> LocalName;
        _ -> return_error(Name, {Ns, "Namespace missmatch"})
    end.   


%%%-----------------------------------------------------------------------------
%%%        SOAP Encoder
%%%-----------------------------------------------------------------------------


%% @doc Create an encoder/1 with the given options.
-spec encoder([encoder_option()]) -> function().
encoder(Options) ->
    State = parse_encoder_options(Options, #encoder{}),
    fun (O) -> soap_encode(O, State) end.

%% @doc Encode the given as SOAP to an rpc_data.
-spec encode(#rpc_data{}) -> #xmlElement{}.
encode(Any) ->
    soap_encode(Any, #encoder{}).

parse_encoder_options([], State) ->
    State;
parse_encoder_options([{version, Version} | Rest], State) ->
    parse_encoder_options(Rest, State#encoder{version=Version});
parse_encoder_options([{handler, Handler} | Rest], State) ->
    parse_encoder_options(Rest, State#encoder{handler=Handler}).


-compile({nowarn_unused_function, return_error/2}).
-spec return_error(term(), any()) -> no_return().
return_error(Tag, Message) ->
    throw({error, {Tag, ?MODULE, Message}}).

-spec soap_encode(#rpc_data{}, #encoder{}) -> #xmlElement{}.
soap_encode(_Any, #encoder{}) ->
    #xmlElement{}.

%%%-----------------------------------------------------------------------------
%%% Unitary tetsts
%%%-----------------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(XML,
	"<soap-env:Envelope " ++
	    "    xmlns:soap-enc=\"http://schemas.xmlsoap.org/soap/encoding/\" " ++
	    "    xmlns:soap-env=\"http://schemas.xmlsoap.org/soap/envelope/\"" ++
	    "    xmlns:cwmp=\"urn:dslforum-org:cwmp-1-0\">" ++
	    "   " ++
	    "  <soap-env:Header>" ++
	    "    <cwmp:ID mustUnderstand=\"1\">42</cwmp:ID>" ++
	    "    <cwmp:NoMoreRequests>0</cwmp:NoMoreRequests>" ++
	    "  </soap-env:Header>" ++
	    "  <soap-env:Body>" ++
	    "    <soap-env:Fault>" ++
	    "      <faultcode>Client</faultcode>" ++
	    "      <faultstring>CWMP fault</faultstring>" ++
	    "      <detail>" ++
	    "	<cwmp:Fault>" ++
	    "	  <FaultCode>9001</FaultCode>" ++
	    "	  <FaultString>Request Denied</FaultString>" ++
	    "	</cwmp:Fault>" ++
	    "      </detail>" ++
	    "    </soap-env:Fault>" ++
	    "  </soap-env:Body>" ++
	    "</soap-env:Envelope>"
       ).

gen_ref_doc() ->
    DocRef = tr_soap_lib:read_xml(?XML),
    DocRef.

dummy(_S) ->
    ok.

rw_xml_test_() ->
    { foreach,
      fun gen_ref_doc/0,
      fun dummy/1,
      [
       fun check_rw_id/1
      ]}.

check_rw_id(DocRef) ->
    fun() ->
	    ?assertEqual(DocRef, read_xml(write_xml(DocRef))) end.

name_namespace_test() ->
    ?assertEqual('name', get_local_name('name', "")),
    ?assertEqual('name', get_local_name('ns:name', "ns")),   
    {Name, Ns} = {'ns:name', "ns"},
    ?assertEqual(Name, get_QName(get_local_name(Name, Ns), Ns)),
    ok.

qname_test() ->
    ?assertEqual('ns:name', get_QName('name', "ns")),
    ?assertEqual('ns:name', get_QName("name", "ns")).
    
-define(WHENEVER, "Hello meck").
meck_test()->
    meck:new(tr_soap),
    meck:expect(tr_soap,
		hello, 0, ?WHENEVER),
						%    ?DBG(tr_soap:hello()),
    meck:unload(tr_soap).

-endif.
