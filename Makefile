
REBAR=rebar

.PHONY: clean compile example

compile:
	@./$(REBAR) compile

clean:
	@./$(REBAR) clean
	@ rm -rf ebin/
	@ rm -rf example/*.beam

dialyzer:
	@dialyzer ebin/ --plt $(HOME)/.dialyzer_plt -Wunmatched_returns \
			-Werror_handling -Wrace_conditions -Wbehaviours \
			-Wunderspecs
edoc:
	@./$(REBAR) doc

edoclean:
	@rm -rf doc

example:
	@cp -r example/templates/* priv/reports/
	@(cd example && erlc sample_pdf.erl && erlc sample_xls.erl)
	@erl -pa ebin/ -eval 'application:start(pdferl), code:add_path("$(shell pwd)"), code:add_patha("example")'
