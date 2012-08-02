%%% Welcome to the Distel Interactive Erlang Shell.
%%
%% C-j evaluates an expression and prints the result in-line.
%% C-M-x evaluates a whole function definition.

inets:start().
% -:-> ok

Caprica = 'http://caprica.mrc.alcatel.ro'.
% -:-> 'http://caprica.mrc.alcatel.ro'


Error: {{error,{2,erl_parse,["syntax error before: ","':'"]}},
 {error,{4,erl_parse,["syntax error before: ",["Caprica"]]}}}

Error: {{error,{2,erl_parse,["syntax error before: ","':'"]}},
 {error,{4,erl_parse,["syntax error before: ",["Caprica"]]}}}


{ok, {{Version, 200, ReasonPhrase}, Headers, Body}} = httpc:request(get, {"http://caprica.mrc.alcatel.ro", []}, [], []).
% -:-> {ok,{{"HTTP/1.1",200,"OK"},
	  [{"connection","Keep-Alive"},
	   {"date","Fri, 13 Jul 2012 06:20:02 GMT"},
	   {"accept-ranges","bytes"},
	   {"etag","\"474df-2d1-4b6bff883aea6\""},
	   {"server","Apache/2.2.16 (Debian)"},
	   {"vary","Accept-Encoding"},
	   {"content-length","721"},
	   {"content-type","text/html"},
	   {"last-modified","Tue, 17 Jan 2012 21:25:31 GMT"},
	   {"keep-alive","timeout=15, max=100"}],
	  "<html><body><h1>It works!</h1>\n<p>This is the default web page for this server.</p>\n<p>Most probably you are looking for this page <a href=\"http://caprica.mrc.alcatel.ro/projects/ib/wiki\">http://caprica.mrc.alcatel.ro/projects/ib/wiki</a></p>\n<p>\n<h3> Caprica is an Earth-like planet settled by the Capricorn tribe of Kobol. Caprica is in the Helios Alpha star system, and has a twin planet called Gemenon that is only 493,000 kilometres (306,336.0 mi) away. The twin planets share a mutual orbit where each planet trades places with the other every 28.2 days. Caprica also shares its star system with Picon and Tauron. It is known throughout the Four Systems as the \"Capital of the Colonies\"  \n</h3>\n</p>\n</body></html>\n"}}

{ok, RequestId} = httpc:request(get, {"http://caprica.mrc.alcatel.ro", []}, [], [{sync, false}]).
% -:-> {ok,#Ref<0.0.0.72>}

receive {http, {RequestId, Result}} -> ok after 500 -> error end.

