%% ==============================================================================
%
% PDFERL
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

-module(pdferl).

%% behaviour
-behaviour(gen_server).

%% export api
-export([start_link/0, create/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% include file
-include("pdferl.hrl").

%% state record
-record(state, {alive_conn}).

%% @doc starts this server 
%% @spec start_link() -> {ok, pid()} | ignore | {error, term()}
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    State = #state{alive_conn = []},
    gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

%% api 

%% @doc creates pdf | xls | rtf from xml
%% @spec create(Args :: tuple()) -> ok
-spec create(Args :: tuple()) -> ok.
create(Args) ->
    gen_server:call(?MODULE, {making, Args}).

%% init     

%% @doc initiates this server
%% @spec init(#state{}) -> {ok, #state{}} | {ok, #state{}, integer()} | ignore
%%			   | {stop, term()}
-spec init(#state{}) -> {ok, {state, #state{}}}.
init(State = #state{alive_conn = _Alive}) ->
    process_flag(trap_exit, true),
    {ok, State}.

%% @doc handling call messages
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
handle_call({making, Args}, From, #state{alive_conn = Alive}) -> 
    {Xml, Xpath, JasperFile, NameFile, TypeFile} = Args,
    PortDrv = pdferl_cfg:get_driver_port({Xpath, JasperFile, NameFile, TypeFile}),
    DataSource = term_to_binary({xml, list_to_binary(Xml)}),
    pdferl_cfg:exec_cmd(PortDrv, DataSource),
    {reply, ok, #state{alive_conn = [{PortDrv, From}] ++ Alive}}.

%% @doc handling cast messages
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
handle_cast(_Msg, State) ->
   {noreply, State}.

%% @doc handling info (all non cast/call) messages
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
handle_info({'EXIT', _PortDrv, _Reason}, #state{alive_conn = _Alive} = State) ->
   {noreply, State};
handle_info({PortDrv, {data, Response}}, #state{alive_conn = Alive}) ->
   [From] = [ Pid || {Pdrv, {Pid, _}} <- Alive, PortDrv =:= Pdrv],
   From ! binary_to_term(Response),
   erlang:port_close(PortDrv),
   {noreply, #state{alive_conn = [ A || {Pdrv, _ }=A <- Alive, Pdrv =/= PortDrv]}}.

%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
terminate(_Reason, _State) ->
   ok.

%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

