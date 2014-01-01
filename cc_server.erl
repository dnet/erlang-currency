-module(cc_server).
-behavior(gen_server).

-record(cc_srv_state, {tab, last_update=never}).
-record(parser_state, {tab, currency, rate, acc}).

-export([start_link/0, convert/4]).
-export([init/1, handle_call/3]).

-define(MAX_AGE, (60 * 60 * 4)).
-define(REGULAR_BRIDGE_CURRENCY, "HUF").

start_link() ->
	{ok, Pid} = gen_server:start_link(?MODULE, [], []),
	Pid.

init([]) ->
	Tab = ets:new(cc_rates, []),
	{ok, do_update(#cc_srv_state{tab=Tab})}.

convert(ServerRef, From, To, Amount) ->
	gen_server:call(ServerRef, {convert, From, To, Amount}).

handle_call({convert, From, To, Amount}, _, CallState) ->
	State = check_update(CallState),
	Response = case get_rate(State#cc_srv_state.tab, From, To) of
		no_rate -> no_rate;
		Rate -> Rate * Amount
	end,
	{reply, Response, State}.

get_rate(_, C, C) -> 1;
get_rate(Tab, From, To) ->
	case ets:lookup(Tab, {From, To}) of
		[{_, Rate}] -> Rate;
		[] ->
			case ets:lookup(Tab, {To, From}) of
				[{_, Rate}] -> 1 / Rate;
				[] -> no_rate
			end
	end;
get_rate(_, _, _) ->
	no_rate.

check_update(#cc_srv_state{last_update={LastMegaSecs, LastSecs, _}} = State) ->
	{NowMegaSecs, NowSecs, _} = os:timestamp(),
	case NowMegaSecs * 1000000 + NowSecs - (LastMegaSecs * 1000000 + LastSecs) > ?MAX_AGE of
		true -> do_update(State);
		false -> State
	end.

do_update(#cc_srv_state{tab=Tab} = State) ->
	{ok, _, _} = xmerl_sax_parser:stream(query_currency_xml(),
		[{event_fun, fun event_handler/3}, {event_state, #parser_state{tab=Tab}}]),
	State#cc_srv_state{last_update=os:timestamp()}.

event_handler({startElement, _, "penznem", _, _}, _, State) -> State#parser_state{acc=""};
event_handler({startElement, _, "kozep", _, _}, _, State) -> State#parser_state{acc=""};
event_handler({endElement, _, "penznem", _}, _, #parser_state{acc=Acc} = State) ->
	State#parser_state{currency=lists:append(lists:reverse(Acc)), acc=undefined};
event_handler({endElement, _, "kozep", _}, _, #parser_state{acc=Acc} = State) ->
	State#parser_state{rate=list_to_float(lists:append(lists:reverse(Acc))), acc=undefined};
event_handler({endElement, _, "item", _}, _, #parser_state{tab=T, currency=C, rate=R}) ->
	ets:insert(T, {{C, ?REGULAR_BRIDGE_CURRENCY}, R}),
	#parser_state{tab=T};
event_handler({characters, Data}, _, #parser_state{acc=Acc} = State) when is_list(Acc) ->
	State#parser_state{acc=[Data | Acc]};
event_handler(_, _, State) -> State.

query_currency_xml() ->
	{ok, {_, _, XML}} = httpc:request("http://api.napiarfolyam.hu/?bank=mnb"),
	XML.
