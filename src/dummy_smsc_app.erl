-module(dummy_smsc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	CliArguments = init:get_argument(smsc),
	case CliArguments of
		{ok, Params} ->
			SmscParams = Params;
		_ ->
			SmscParams = []
	end,
    dummy_smsc_sup:start_link(params:normalize_keys(SmscParams)).

stop(_State) ->
    ok.
