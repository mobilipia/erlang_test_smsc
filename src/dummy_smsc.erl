-module(dummy_smsc).

-export([start/0]).

start()->
	application:start(sasl),
	application:start(log4erl),
	{ok, Args} = init:get_argument(smsc),
	[Log] = [[Y] || ["log",Y] <- Args],
	case Log of
		[LogDir] ->
			LogDir;
		_ ->
			LogDir = "log"
	end,
	log4erl:add_file_appender(file,{LogDir, "dummy_smsc", {size, 100000}, 4, "elog", debug}),
	log4erl:change_format(file, "%Y-%M-%D %T [%L] %l%n"),
	application:start(dummy_smsc).