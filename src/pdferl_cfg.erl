%% ==============================================================================
%
% PDFERL CFG
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

-module(pdferl_cfg).

%% include file
-include("pdferl.hrl").

%% export
-export([config/1, get_driver_port/1, exec_cmd/2]).

%% @doc loads config from config file to the system
%% @spec config(File :: string()) -> [] | ok
-spec config(File :: string()) -> [] | ok.
config(File) ->
    case file:consult(File) of
        {error, _FileError} -> 
	    [];
        {ok, Vars}         ->
	    Path = proplists:get_value(path_to_ruby_src, Vars),
            application:set_env(pdferl, path_to_ruby_src, Path)
    end.

%% @doc get port driver for command
%% @spec get_driver_port(Cmd :: string() | {Xpath :: string(),
%%					   JasperFile :: string(),
%%					   NameFile :: string(),
%%					   TypeFile :: string()}) -> port() 
-spec get_driver_port(Cmd :: string() | {Xpath :: string(), 
					 JasperFile :: string(),
					 NameFile :: string(), 
					 TypeFile :: string()}) -> port().
get_driver_port({Xpath, JasperFile, NameFile, TypeFile}) ->
    {ok, Path} = application:get_env(pdferl, path_to_ruby_src),
    get_driver_port(?cmd(Xpath, JasperFile, NameFile, TypeFile, Path));
get_driver_port(Cmd)                                     ->
   erlang:open_port({spawn, Cmd}, ?port_options).

%% @doc execute command through connected port 
%% @spec exec_cmd(PortDrv :: port(), DataSource :: binary()) -> term()
-spec exec_cmd(PortDrv :: port(), DataSource :: binary()) -> term().
exec_cmd(PortDrv, DataSource) ->
    port_command(PortDrv, DataSource).    
