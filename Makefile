TR_VSN = $(shell sed -n 's/.*{vsn,.*"\(.*\)"}.*/\1/p' src/tr.app.src)
REBAR='./rebar'
# || which rebar`

all:
	$(REBAR) -v compile

clean:
	$(REBAR) clean 

dist-clean:
	$(REBAR) clean delete-deps

install: all
	mkdir -p $(DESTDIR)/lib/tr-$(IBROWSE_VSN)/
	cp -r ebin $(DESTDIR)/lib/tr-$(IBROWSE_VSN)/

test: all
	$(REBAR) skip_deps=true eunit

# (cd test; make) 
# erl -noshell -pa ebin -pa test -s tr -s tr_test unit_tests \
# -s tr_test verify_chunked_streaming \
# -s tr_test test_chunked_streaming_once \
# -s erlang halt

dialyzer-build:
	dialyzer --build_plt --verbose			\
	  --output_plt .dialyzer-R15B.plt 		\
	  --apps kernel stdlib sasl erts ssl 	 	\
	    tools os_mon runtime_tools crypto 		\
	    inets xmerl public_key syntax_tools 	\
	    mnesia eunit et compiler			\
	    ./deps/*/ebin

dialyzer: all
	dialyzer --plt .dialyzer-R15B.plt \
	  -Wunmatched_returns 	\
	  -Werror_handling 	\
	  -Wrace_conditions 	\
	  -Wunderspecs		\
	  ./ebin

#	  --src ./src

#	  -Wbehaviours 		\
