
-module(sample_pdf).

-define(xpath, "/document/personas/persona").
-define(jasper_file, "frame.jasper").
-define(name_file, "example_pdf").
-define(type_file, "pdf").

-export([make/0]).

make() ->
    Args = {xml(), ?xpath, ?jasper_file, ?name_file, ?type_file},
    ok = pdferl:create(Args),
    do_recv().

do_recv() ->
    receive
        X ->
	    X
    end.

xml() ->
    TagPerson = [{nombre, "name here!"}, {apellido, "last name here!"}, {edad, "00"}],
    xml:set_header(
        xml:encode([{document, [{personas, [{persona, TagPerson}]}]}])).
