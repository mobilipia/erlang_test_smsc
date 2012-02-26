-module(smsc).

-behaviour(gen_smsc).

-include_lib("oserl/include/oserl.hrl").
-include("records.hrl").

-export([start_link/0, start_link/1, info/0]).

-export([init/1,
         handle_bind/3,
         handle_operation/3,
         handle_unbind/3,
         handle_listen_error/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_SYSTEM_ID, "test").
-define(DEFAULT_PSWD, "test").
-define(DEFAULT_PORT, ?DEFAULT_SMPP_PORT).
-define(DEFAULT_HOST, {127,0,0,1}).

start_link()->
	start_link([]).

start_link([]) ->
    gen_smsc:start_link({local, ?SERVER}, ?MODULE, [[]], []);
start_link([Args]) ->
    gen_smsc:start_link({local, ?SERVER}, ?MODULE, [[Args]], []).

info()->
	gen_server:call(?SERVER, info).

init([Params]) ->
    {
		ok,
		#dummy_smsc_state{
			smsc_params = [
				{system_id, params:get(Params, system_id, ?DEFAULT_SYSTEM_ID)},
				{password, params:get(Params, password, ?DEFAULT_PSWD)}
			]
		}
	}.

handle_bind({_CmdName, _Session, _Pdu, _IpAddr}, _From, State) ->
    ParamList = State#dummy_smsc_state.smsc_params,
    {reply, {ok, ParamList}, State}.

handle_operation({_CmdName, _Session, _Pdu}, _From, S) ->
    % Don't know how to handle CmdName
    {reply, {error, ?ESME_RINVCMDID, []}, S}.

handle_unbind({unbind, _Session, _Pdu}, _From, State) -> 
    {reply, ok, State}.

handle_listen_error(State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
