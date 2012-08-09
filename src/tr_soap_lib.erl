%%% File    : tr_soap_lib.erl
%%% Description : SOAP import/export routines


-module(tr_soap_lib).

-include_lib("xmerl/include/xmerl.hrl").
-include("proto.hrl").

-export([encoder/1, encode/1]).
-export([decoder/1, decode/1]).


-type encoder_option() :: {version, 1 | 2 | 3} | {handler, function()}.
-type decoder_option() :: {version, 1 | 2 | 3} | {object_hook, function()}.

-record(encoder, {version=1,
                  handler=null}).

-record(decoder, {version=1,
                  object_hook=null,
                  current_xml_element :: #xmlElement{},
                  state=null}).

%% @doc Create an encoder/1 with the given options.
-spec encoder([encoder_option()]) -> function().
encoder(Options) ->
    State = parse_encoder_options(Options, #encoder{}),
    fun (O) -> soap_encode(O, State) end.

%% @doc Encode the given as JSON to an iolist.
-spec encode(#rpc_data{}) -> #xmlElement{}.
encode(Any) ->
    soap_encode(Any, #encoder{}).


%% @doc Create a decoder/1 with the given options.
-spec decoder([decoder_option()]) -> function().
decoder(Options) ->
    State = parse_decoder_options(Options, #decoder{}),
    fun (O) -> soap_decode(O, State) end.

%% @doc Decode the given iolist to Erlang terms.
-spec decode([#xmlElement{}]) -> #rpc_data{}.
decode(S) ->
    soap_decode(S, #decoder{}).



%%%-----------------------------------------------------------------------------
%% Internal API
%%%-----------------------------------------------------------------------------

parse_encoder_options([], State) ->
    State;
parse_encoder_options([{version, Version} | Rest], State) ->
    parse_encoder_options(Rest, State#encoder{version=Version});
parse_encoder_options([{handler, Handler} | Rest], State) ->
    parse_encoder_options(Rest, State#encoder{handler=Handler}).

parse_decoder_options([], State) ->
    State;
parse_decoder_options([{version, Version} | Rest], State) ->
    parse_decoder_options(Rest, State#decoder{version=Version});
parse_decoder_options([{object_hook, Hook} | Rest], State) ->
    parse_decoder_options(Rest, State#decoder{object_hook=Hook}).

%%%-----------------------------------------------------------------------------
%%%        SOAP Encoder
%%%-----------------------------------------------------------------------------

-spec soap_encode(#rpc_data{}, #encoder{}) -> #xmlElement{}.
soap_encode(_Any, #encoder{}) ->
    #xmlElement{}.

%%%-----------------------------------------------------------------------------
%%%        SOAP Decoder
%%%-----------------------------------------------------------------------------


-spec soap_decode([#xmlElement{}], #decoder{}) -> #rpc_data{}.
soap_decode(_LE, _S) ->
    #rpc_data{}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec soap_decode([#xmlElement{}], #decoder{}) -> #rpc_data{}.
soap_decode(LS, S) ->
    [
     case parse_element(E, S) of
	 {}
	 end     
     || E <- LS],
    #rpc_data{header = H, type = T, message = M}.

parse_element(E = #xmlElement{name='soap:Envelope'}, S) ->
    ok. 
-endif.
