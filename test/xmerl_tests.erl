%%% File    : xmerl_tests.erl
%%% Description : 


-module(xmerl_tests).

-include_lib("xmerl/include/xmerl.hrl"). 

-import(xmerl_xs, 
	[ xslapply/2, value_of/1, select/2, built_in_rules/2 ]). 

-include_lib("eunit/include/eunit.hrl").
-include("tr69.hrl").


parse_URI_test () ->
    ?assertEqual({http,"acs-host-name",80,"/w",[]},
		 xmerl_uri:parse("http://acs-host-name/w")),    
    ?assertEqual({http,"cpe-host-name",80,"/kick.html","?command=cmd&arg=1&next=home"},
		 xmerl_uri:parse("http://cpe-host-name/kick.html?command=cmd&arg=1&next=home")),
    
    ok.

parse_SOAP_simple_test() ->
    DataDir = os:getenv("PWD"),
    {E,[]} = xmerl_scan:file(filename:join([DataDir,test,"soap_simple.xml"])),
    ?DEBUG(E),
    E.

