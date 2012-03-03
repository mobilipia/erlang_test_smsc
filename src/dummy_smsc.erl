-module(dummy_smsc).

-export([start/0]).

start()->
	application:start(sasl),
	application:start(log4erl),
	log4erl:add_file_appender(file,{"log", "dummy_smsc", {size, 100000}, 4, "elog", debug}),
	log4erl:change_format(file, "%Y-%M-%D %T [%L] %l%n"),
	application:start(dummy_smsc).