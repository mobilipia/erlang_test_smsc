
-module(dummy_smsc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link,[]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link()->
	log4erl:debug("Starting smsc supervisor"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	log4erl:debug("Initializing smsc supervisor"),
	{ok, 
		{
			{one_for_one, 5, 10}, 
			[
				?CHILD(app_property, worker),
				?CHILD(smsc, worker)
			]
		} 
	}.

