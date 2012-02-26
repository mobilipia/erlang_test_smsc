-module(params).

-export([get/2, get/3, set/3, normalize_keys/1]).

get(List, ParamName)->
	case lists:keyfind(ParamName, 1, List) of
		{_, ParamValue} ->
			ParamValue;
		_ ->
			false
	end.

get(List, ParamName, DefaultValue)->
	case get(List, ParamName) of
		false -> DefaultValue;
		Value -> Value
	end.
	
get_params_list(List, ParamNameList)->
	%% need to implement
	ok.

normalize_keys(List)->
	normalize_keys(List,[]).

normalize_keys([[Key,Value]|Tail],ResultList)->
	AtomicKey = try	list_to_existing_atom(Key)
	catch
		error:_ -> list_to_atom(Key)
	end,
	normalize_keys(Tail, ResultList++[{AtomicKey,Value}]);
normalize_keys([],ResultList)->
	lists:reverse(ResultList).

set(List, ParamName, Value)->
	case get(List,ParamName) of
		false ->
			[{ParamName,Value}|List];
		_ ->
			lists:keyreplace(ParamName, 1, List, {ParamName,Value})
	end.