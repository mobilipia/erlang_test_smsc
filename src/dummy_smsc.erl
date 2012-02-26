-module(dummy_smsc).

-export([start/0]).

start()->
	application:start(sasl),
	application:start(dummy_smsc).