
REBAR = rebar

.PHONY: clean compile example

compile:
	@./$(REBAR) compile

clean:
	@./$(REBAR) clean
	@ rm -rf ebin/
	@ rm -rf example/*.beam

start:
	@erl -pa ebin/ -eval 'application:start(pdferl)'

example:
	@cp -r example/templates/* reports/
	@(cd example && erlc sample_pdf.erl && erlc sample_xls.erl)
	@erl -pa ebin/ -eval 'application:start(pdferl), code:add_patha("example")'
