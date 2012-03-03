-module(dummy_smsc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	log4erl:debug("Starting smsc application"),
    dummy_smsc_sup:start_link().

stop(_State) ->
	log4erl:debug("Application stopped"),
    ok.
