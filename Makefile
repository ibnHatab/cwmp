TR_VSN = $(shell sed -n 's/.*{vsn,.*"\(.*\)"}.*/\1/p' src/tr.app.src)

all:
	./rebar compile

clean:
	./rebar clean 

dist-clean:
	./rebar clean delete-deps

install: all
	mkdir -p $(DESTDIR)/lib/tr-$(IBROWSE_VSN)/
	cp -r ebin $(DESTDIR)/lib/tr-$(IBROWSE_VSN)/

test: all
	./rebar eunit
	(cd test; make) 
	erl -noshell -pa ebin -pa test -s tr -s tr_test unit_tests \
	-s tr_test verify_chunked_streaming \
	-s tr_test test_chunked_streaming_once \
	-s erlang halt

