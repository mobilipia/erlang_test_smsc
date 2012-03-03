-module(app_property).

-behaviour(gen_server).

-export([start_link/0, set/2, get/1, get/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).
-export([stop/0, terminate/2]).

-define(SERVER, ?MODULE).

set(Name, Value)->
	gen_server:cast(?SERVER, {set, Name, Value}).

get(Name)->
	gen_server:call(?SERVER, {get, Name}).
get(Name, DefaultValue)->
	Val = gen_server:call(?SERVER, {get, Name}),
	case Val of
		false ->
			DefaultValue;
		_ ->
			Val
	end.

start_link() ->
	log4erl:debug("Starting app property"),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
	log4erl:debug("Stopping app_property"),
  gen_server:cast(?SERVER, stop).

init([]) ->
	log4erl:debug("Initializing app property"),
	PropertyTable = ets:new(property_table,[]),
	{ok, [PropertyTable],0}.

handle_call({get, Name}, _From, [Table]) ->
	Obj = ets:lookup(Table, Name),
	case Obj of
		[{Name, Value}] ->
			Return = Value;
		_ ->
			Return = false
	end,
	{reply, Return, [Table]}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast({set, Name, Value}, [Table]) ->
	ets:insert(Table, {Name, Value}),
	{noreply, [Table]}.

handle_info(timeout, State) ->
	{ok, CliArguments} = init:get_argument(smsc),
	Args = params:normalize_keys(CliArguments),
	log4erl:debug("Filling app properties with ~p",[Args]),
	lists:foreach(
		fun({Name, Value}) ->
			app_property:set(Name, Value)
		end, Args),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, _State) ->
  ok;
terminate(shutdown, _State) ->
  ok;
terminate({shutdown, _Reason}, _State) ->
  ok;
terminate(_Reason, _State) ->
  ok.