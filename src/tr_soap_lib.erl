%%% File    : tr_soap_lib.erl
%%% Description : SOAP import/export routines


-module(tr_soap_lib).

-include_lib("xmerl/include/xmerl.hrl").

-include("tr69.hrl").
-include("proto.hrl").

-export([encoder/1, encode/1]).
-export([decoder/1, decode/1]).

-export([read_xml/1, write_xml/1]).

-type encoder_option() :: {version, 1 | 2 | 3} | {handler, function()}.
-type decoder_option() :: {version, 1 | 2 | 3} | {object_hook, function()}.

-record(encoder, {version=1,
                  handler=null}).

-record(rpc_ns, {ns_xsd = "",
                 ns_envelop = "",
                 ns_cwmp = ""}).

-record(decoder, {version=1,
                  object_hook=null,
                  state=null,
                  ns :: #rpc_ns{}
                 }).

%% @doc Create an encoder/1 with the given options.
-spec encoder([encoder_option()]) -> function().
encoder(Options) ->
    State = parse_encoder_options(Options, #encoder{}),
    fun (O) -> soap_encode(O, State) end.

%% @doc Encode the given as SOAP to an rpc_data.
-spec encode(#rpc_data{}) -> #xmlElement{}.
encode(Any) ->
    soap_encode(Any, #encoder{}).


%% @doc Create a decoder/1 with the given options.
-spec decoder([decoder_option()]) -> function().
decoder(Options) ->
    State = parse_decoder_options(Options, #decoder{}),
    fun (O) -> soap_decode(O, State) end.

%% @doc Decode the given xmlElement to rpc_data terms.
%-spec decode([#xmlElement{}]) -> #envelope{}.% | {error, any()}.
-spec decode(#xmlElement{}) -> #rpc_data{}.
decode(S) ->
%    #envelope{}.
    soap_decode(S, #decoder{}).

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
-spec parse_encoder_options([Option],#encoder{}) -> #encoder{} when
      Option :: {'handler', term()} | {'version', cwmp_version()}.
parse_encoder_options([], State) ->
    State;
parse_encoder_options([{version, Version} | Rest], State) ->
    parse_encoder_options(Rest, State#encoder{version=Version});
parse_encoder_options([{handler, Handler} | Rest], State) ->
    parse_encoder_options(Rest, State#encoder{handler=Handler}).

-spec parse_decoder_options([Option], #decoder{}) -> #decoder{} when
      Option :: {'object_hook', term()} | {'version', cwmp_version()}.
parse_decoder_options([], State) ->
    State;
parse_decoder_options([{version, Version} | Rest], State) ->
    parse_decoder_options(Rest, State#decoder{version=Version});
parse_decoder_options([{object_hook, Hook} | Rest], State) ->
    parse_decoder_options(Rest, State#decoder{object_hook=Hook}).


-compile({nowarn_unused_function, return_error/2}).
-spec return_error(term(), any()) -> no_return().
return_error(Tag, Message) ->
    throw({error, {Tag, ?MODULE, Message}}).

%%%-----------------------------------------------------------------------------
%%%        SOAP Encoder
%%%-----------------------------------------------------------------------------

-spec soap_encode(#rpc_data{}, #encoder{}) -> #xmlElement{}.
soap_encode(_Any, #encoder{}) ->
    #xmlElement{}.

%%%-----------------------------------------------------------------------------
%%%        SOAP Decoder
%%%-----------------------------------------------------------------------------

-spec parse_error(#xmlElement{}, #decoder{}) -> no_return().
parse_error(Elem, State) ->
    return_error(Elem#xmlElement.name,
                 {State#decoder.state, "Unknown element"}).

-spec soap_decode(#xmlElement{}, #decoder{}) -> #rpc_data{}.
%-spec soap_decode(#xmlElement{}, #decoder{}) -> #envelope{} | {error, any()}.
soap_decode(Doc, S) ->
    try
        parseMessage(Doc, S)
     catch
         error: Error ->
            Stacktrace = erlang:get_stacktrace(),
             erlang:raise(error, Error, Stacktrace)
         %% Probably thrown from return_error/2:
         %% throw: {error, {Tag, ?MODULE, M}} ->
         %%     Stacktrace = erlang:get_stacktrace(),
         %%     erlang:raise(error, {Tag, M}, Stacktrace)
     end.

-spec get_qualified_name(atom(),string()) -> atom().
-compile({nowarn_unused_function, get_qualified_name/2}).
get_qualified_name(Name, Ns) when is_atom(Name)->    
    list_to_atom(Ns ++ ":" ++ atom_to_list(Name)).

-spec get_local_name(atom(), string()) -> atom().
get_local_name(Name, Ns) when is_atom(Name), Ns =:= "" ->
    Name;
get_local_name(Name, Ns) when is_atom(Name)->
    case re:split(atom_to_list(Name), ":", [{return,list}]) of
        [Ns, LocalName] -> 
            list_to_atom(LocalName);
        _ -> return_error(Name, {Ns, "Namespace missmatch"})
    end.   

-spec parseMessage(#xmlElement{}, #decoder{}) -> #rpc_data{}.
parseMessage(#xmlElement{namespace = Namespace} = Doc, State) when is_tuple(Doc) ->
    {Nss, Version} = parseNamespace(Namespace),
    RefinedState = State#decoder{version=Version, ns=Nss, state=soap},
    Envelop = case get_local_name(Doc#xmlElement.name, Nss#rpc_ns.ns_envelop) of
                  'Envelope' ->
                      parseEnvelope(Doc, RefinedState);
                  _ ->
                      parse_error(Doc, RefinedState)
              end,
    #rpc_data{data = Envelop}.

-spec parseNamespace(#xmlNamespace{}) -> {#rpc_ns{}, cwmp_version()}.
parseNamespace(#xmlNamespace{nodes = Nss}) ->
    {NsCwmp, CwmpVersion} = match_cwmp_ns_and_version(Nss),
    {#rpc_ns{ns_xsd = find_ns_aux(Nss, ?XSD_URL),
             ns_envelop = find_ns_aux(Nss, ?SOAPENV_URL),
             ns_cwmp = NsCwmp},
     CwmpVersion}.

-spec match_cwmp_ns_and_version([tuple()]) -> {string(), cwmp_version()}.
match_cwmp_ns_and_version(Nss) ->
    Mapped = lists:map(fun({Ns, Uri}) ->
                               case re:split(atom_to_list(Uri), "-", [{return,list}]) of
                                   ["urn:dslforum","org:cwmp", R, V] -> {Ns, R, V};
                                   _ -> false
                               end
                       end, Nss),
    Filtered = lists:filter(fun(X) when X == false -> false;
                               (_) -> true end, Mapped),
    {NsCwmp, CwmpVersion} =  case Filtered of
                                 [{NsC,"1","0"}] -> {NsC, 1};
                                 [{NsC,"1","2"}] -> {NsC, 2};
                                 _ -> {"", 1}
                             end,
    {NsCwmp, CwmpVersion}.

-spec find_ns_aux([tuple()], term()) -> string().
find_ns_aux(Nss, URL) ->
    case lists:keyfind(URL, 2, Nss) of
        {NsX, _} ->
            NsX;
        false ->
            ""
    end.

-spec parseEnvelope(#xmlElement{}, #decoder{}) -> #envelope{}.
parseEnvelope(#xmlElement{content = Content} = _Doc,
              #decoder{ns=Nss} = State) ->
    Result = lists:foldl(fun(Elem, Envelop) ->
                                 case get_local_name(Elem, Nss#rpc_ns.ns_envelop) of
                                     'Header' ->
                                         Header = parseHeader(Elem, State),
                                         Envelop#envelope{header = Header};
                                     %% 'Body' ->
                                     %%     Body = parseBody(Elem, State),
                                     %%     Envelop#envelope{body = Body};
                                     _ ->
                                         parse_error(Elem, State)
                                 end
                         end, #envelope{}, Content),
    Result.


-spec parseHeader(atom(),#decoder{}) -> #header{}.
parseHeader(_Elem, _State) ->
    #header{}.

%-spec parseBody(#xmlElement{}, #decoder{}) -> body_type().
%% parseBody(Elem, State) ->
%%     ok.

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
    DocRef = read_xml(?XML),
    DocRef.

dummy(_) ->
    ok.

rw_xml_test_() ->
    { foreach,
      fun gen_ref_doc/0,
      fun dummy/1,
      [
       fun check_rw_id/1,
       fun decode_root/1,
       fun get_ns_orts/1
      ]}.

check_rw_id(DocRef) ->
    fun() ->
	    ?assertEqual(DocRef, read_xml(write_xml(DocRef))) end.


decode_root(Doc) ->
    fun() ->
	    Rpc = soap_decode(Doc, #decoder{}),
	    ?DBG(Rpc)
    end.

get_ns_orts(#xmlElement{namespace = Namespace} = Doc) ->
    fun () ->
            {Nss, CwmpVersion} = parseNamespace(Namespace),
            ?assertEqual("", Nss#rpc_ns.ns_xsd),
            ?assertEqual("soap-env", Nss#rpc_ns.ns_envelop),
            ?assertEqual("cwmp", Nss#rpc_ns.ns_cwmp),
            ok
    end.

name_namespace_test() ->
    ?assertEqual('name', get_local_name('name', "")),
    ?assertEqual('name', get_local_name('ns:name', "ns")),
    {Name, Ns} = {'ns:name', "ns"},
    ?DBG(get_local_name(Name, Ns)),
    ?DBG(get_qualified_name('name', "ns")),
%    ?assertEqual(Name, get_qualified_name(get_local_name(Name, Ns), Ns)),
    ok.

-define(WHENEVER, "Hello meck").
meck_test_no()->
    meck:new(tr_soap),
    meck:expect(tr_soap,
		hello, 0, ?WHENEVER),
    ?DBG(tr_soap:hello()),
    meck:unload(tr_soap).

-endif.
