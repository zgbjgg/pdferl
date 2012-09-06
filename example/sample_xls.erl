%% ==============================================================================
%
% SAMPLE XLS CREATION
%
% Copyright (c) 2012 Jorge Garrido <jorge.garrido@morelosoft.com>.
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions
% are met:
% 1. Redistributions of source code must retain the above copyright
%    notice, this list of conditions and the following disclaimer.
% 2. Redistributions in binary form must reproduce the above copyright
%    notice, this list of conditions and the following disclaimer in the
%    documentation and/or other materials provided with the distribution.
% 3. Neither the name of copyright holders nor the names of its
%    contributors may be used to endorse or promote products derived
%    from this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
% ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
% TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
% PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL COPYRIGHT HOLDERS OR CONTRIBUTORS
% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
%% ===============================================================================

-module(sample_xls).

-define(xpath, "/document/personas/persona").
-define(jasper_file, "frame.jasper").
-define(name_file, "example_xls").
-define(type_file, "xls").

-export([make/0]).

%% @doc create a report in pdf format
%% @spec make() -> term()
-spec make() -> term().
make() ->
    Args = {xml(), ?xpath, ?jasper_file, ?name_file, ?type_file},
    ok = pdferl:create(Args),
    do_recv().

%% @doc simple receive to get response from pdferl process
%% @spec do_recv() -> term()
-spec do_recv() -> ok_created_succesfull | term().
do_recv() ->
    receive
        {response, <<"ok">>} -> ok_created_succesfull
    end.

%% @doc converts proplist into xml format to be process by pdferl,
%%      the proplist depends format xml input when the jasper template
%%      was created
%% @spec xml() -> string()
-spec xml() -> string().
xml() ->
    TagPerson = [{nombre, "name here!"}, {apellido, "last name here!"}, {edad, "00"}],
    xml:set_header(
        xml:encode([{document, [{personas, [{persona, TagPerson}]}]}])).
