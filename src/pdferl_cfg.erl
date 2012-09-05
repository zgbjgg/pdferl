
-module(pdferl_cfg).

-include("pdferl.hrl").

-export([config/1, get_driver_port/1, exec_cmd/2]).

config(File) ->
    case file:consult(File) of
        {error, _FileError} -> 
	    [];
        {ok, Vars}         ->
	    Path = proplists:get_value(path_to_ruby_src, Vars),
            application:set_env(pdferl, path_to_ruby_src, Path)
    end.

get_driver_port({Xpath, JasperFile, NameFile, TypeFile}) ->
    {ok, Path} = application:get_env(pdferl, path_to_ruby_src),
    get_driver_port(?cmd(Xpath, JasperFile, NameFile, TypeFile, Path));
get_driver_port(Cmd)                                     ->
   erlang:open_port({spawn, Cmd}, ?port_options).

exec_cmd(PortDrv, DataSource) ->
    port_command(PortDrv, DataSource).    
