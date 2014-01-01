-module(cc_jimmbot).
-export([ircmain/1, ircproc/2, reload/3]).

ircmain(Contact) ->
	Srv = cc_server:start_link(),
	Pid = spawn(?MODULE, ircproc, [Contact, Srv]),
	Contact ! {subscribe, Pid},
	Pid.

reload(Contact, Srv, Pid) ->
	Pid ! reloaded,
	ircproc(Contact, Srv).

ircproc(Contact, Srv) ->
	receive
		quit -> quit;
		{incoming, Data} ->
			S = binary_to_list(Data),
			case string:str(S, "-cc") of
				0 -> nop;
				_ -> spawn(fun() ->
					Contact ! {announce, recognize_cmd(S, Srv)} end)
			end,
			case string:str(S, "-btc") of
				0 -> nop;
				_ -> Contact ! {announce, "use -cc for Bitcoin as well"}
			end,
			ircproc(Contact, Srv);
		{ident, Pid} ->
			Pid ! {ident, "cc_jimmbot"},
			ircproc(Contact, Srv);
		{reload, Pid} ->
			?MODULE:reload(Contact, Srv, Pid);
		_ -> ircproc(Contact, Srv)
	end.

recognize_cmd(Input, Srv) ->
	{match, [UserAmount, From | Rest]} = re:run(Input,
		"-cc( [0-9\\.]+)? ([A-Z]+)(?: in)?( [A-Z]+)?",
		[{capture, all_but_first, list}, caseless]),
	Amount = case UserAmount of
		[32 | AmountStr] ->
			list_to_float(
				case string:str(AmountStr, ".") of
					0 -> AmountStr ++ ".0";
					_ -> AmountStr
				end
			);
		[] -> 1.0
	end,
	To = case Rest of
		[] -> "HUF";
		[[32 | Currency]] -> Currency
	end,
	report_currency(Srv, Amount, string:to_upper(From), string:to_upper(To)).

report_currency(Srv, Amount, From, To) ->
	lists:flatten(case cc_server:convert(Srv, From, To, Amount) of
		no_rate -> io_lib:format("I cannot convert from ~s to ~s :(", [From, To]);
		Result -> io_lib:format("~.2f ~s = ~.2f ~s", [Amount, From, Result, To])
	end).
