
-module(pdferl).

-behaviour(gen_server).

-export([start_link/0, create/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("pdferl.hrl").

-record(state, {alive_conn}).

start_link() ->
    State = #state{alive_conn = []},
    gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

%% api 
create(Args) ->
    gen_server:call(?MODULE, {making, Args}).

%% init     
init(State = #state{alive_conn = _Alive}) ->
    process_flag(trap_exit, true),
    {ok, State}.

handle_call({making, Args}, From, #state{alive_conn = Alive}) -> 
    {Xml, Xpath, JasperFile, NameFile, TypeFile} = Args,
    PortDrv = pdferl_cfg:get_driver_port({Xpath, JasperFile, NameFile, TypeFile}),
    DataSource = term_to_binary({xml, list_to_binary(Xml)}),
    pdferl_cfg:exec_cmd(PortDrv, DataSource),
    {reply, ok, #state{alive_conn = [{PortDrv, From}] ++ Alive}}.

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info({'EXIT', _PortDrv, Reason}, #state{alive_conn = _Alive} = State) ->
   {noreply, State};
handle_info({PortDrv, {data, Response}}, #state{alive_conn = Alive}) ->
   {response, <<"ok">>} = binary_to_term(Response),
   [From] = [ Pid || {Pdrv, {Pid, _}} <- Alive, PortDrv =:= Pdrv],
   From ! <<"ok">>,
   erlang:port_close(PortDrv),
   {noreply, #state{alive_conn = Alive -- [PortDrv]}}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

