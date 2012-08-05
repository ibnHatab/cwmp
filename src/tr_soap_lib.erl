%%% File    : tr_soap_lib.erl
%%% Description : SOAP import/export routines


-module(tr_soap_lib).

-include_lib("xmerl/include/xmerl.hrl").

-export([import/1, export/1]).

%-type xmlElement() :: #xmlElement{}.

%-spec(import(xmlElement()) -> ,

export(_SOAP_Envelop) ->
    ok.

import(_CWMP_Method) ->
    ok.
