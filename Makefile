
#TR_VSN = $(shell sed -n 's/.*{vsn,.*"\(.*\)"}.*/\1/p' src/tr.app.src)
REBAR='./rebar'
# || which rebar`


all: compile

compile: deps
	$(REBAR) -v compile

app:
	$(REBAR) -v compile skip_deps=true

deps:
	$(REBAR) check-deps || $(REBAR) get-deps

clean:
	$(REBAR) clean
	@rm -rf logs/*
	@rm -rf test/*.beam
	@rm -rf .eunit/*.beam

dist-clean:
	$(REBAR) clean delete-deps

install: all
	mkdir -p $(DESTDIR)/lib/tr-$(IBROWSE_VSN)/
	cp -r ebin $(DESTDIR)/lib/tr-$(IBROWSE_VSN)/

utest:
	$(REBAR) -v eunit skip_deps=true suite=tr_soap_types

ut-shell:
	exec erl -pa $(PWD)/apps/*/ebin -pa $(PWD)/deps/*/ebin -pa $(PWD)/.eunit -boot start_sasl -s reloader 


test.spec: test.spec.in
	cat test.spec.in | sed -e "s,@PATH@,$(PWD)," > $(PWD)/test.spec

ctest:  test.spec app
	-@mkdir logs
	ct_run -pa $(PWD)/lib/*/ebin -pz ./ebin -spec test.spec

ct-shell:
	ct_run -shell -pa $(PWD)/lib/*/ebin -pz ./ebin -spec test.spec


test: app
	$(REBAR) -v ct skip_deps=true suites=hdm_trace case=hdm_trace_test_case

dialyzer-build:
	dialyzer --build_plt --verbose			\
	  --output_plt .dialyzer-R15B.plt 		\
	  --apps kernel stdlib sasl erts ssl 	 	\
	    tools os_mon runtime_tools crypto 		\
	    inets xmerl public_key syntax_tools 	\
	    mnesia eunit et compiler			\
	    ./deps/*/ebin

dialyzer: compile
	dialyzer --plt .dialyzer-R15B.plt \
	  -Wunmatched_returns 	\
	  -Werror_handling 	\
	  -Wrace_conditions 	\
	  ./ebin

#	  -Wunderspecs		\
# hardcheck
#	  -Wspecdiffs		\
#	  -Woverspecs 		\

.PHONY: check-data

DATA_XML=$(wildcard test/data/*.xml) $(wildcard test/hdm_trace_SUITE_data/*.xml)

check-data: $(DATA_XML)
	@for file in $(DATA_XML) ; do 	\
		echo ; 			\
		echo "-------------- $$file ----------------" ; \
		xmllint --noout --path doc --schema cwmp-1-1.xsd $$file; \
	done

cmd:
	erl -pa ebin -s tr_soap_parser parse_root_test test/data/Fault.xml -run init stop -noshell

.PHONY: ctest

dir-local: app
