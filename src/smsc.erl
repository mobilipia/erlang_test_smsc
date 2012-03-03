-module(smsc).

-behaviour(gen_smsc).

-include_lib("oserl/include/oserl.hrl").
-include("records.hrl").

-export([start_link/0]).

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
    gen_smsc:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
	Self = self(),
	proc_lib:spawn(fun() ->
		Result = gen_smsc:listen_start(Self,app_property:get(port, ?DEFAULT_PORT), infinity, ?DEFAULT_SMPP_TIMERS),
		gen_smsc:cast(Self,{listen,Result})
	end),
    {
		ok,
		#dummy_smsc_state{
			smsc_params = [
				{system_id, app_property:get(system_id)},
				{password, app_property:get(password)}
			],
			message_id=0
		}
	}.

handle_bind({_CmdName, _Session, Pdu, IpAddr}, _From, State) ->
	io:format("Try to bind from ~p with ~p~n",[IpAddr, dict:to_list(Pdu)]),
    ParamList = State#dummy_smsc_state.smsc_params,
    {reply, {ok, ParamList}, State}.
	%%{reply, {error, ?ESME_RINVSYSID, []}, State}.

handle_operation({CmdName, _Session, Pdu}, _From, State) ->
	NewMessageId = State#dummy_smsc_state.message_id+1,
	io:format("Got ~p with ~p~n",[CmdName, dict:to_list(Pdu)]),
	Message = dict:fetch(short_message, Pdu),
	case sm:udhi(Pdu) of
		true ->
			{_Udh, Rest} = sm:chop_udh(Message);
		_ ->
			Rest = Message
	end,
	io:format("Message ~p~n",[Rest]),
	{reply, {ok, [{message_id, integer_to_list(NewMessageId)}]}, State#dummy_smsc_state{message_id=NewMessageId}}.

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