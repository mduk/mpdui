-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-include( "mpdui.hrl" ).

-export( [ init/3, websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3 ] ).

init( { tcp, http }, _Req, _Opts ) ->
	{ upgrade, protocol, cowboy_websocket }.

websocket_init( _, Req, _ ) ->
	{ ok, C } = erlmpd:connect(),
	mpdui_poller_currentsong:start_link( self(), C ),
	{ ok, Req, C }.

websocket_handle( { text, RawMsg }, Req, C ) ->
	{ struct, MsgData } = mochijson2:decode( RawMsg ),
	Command = #mpdui_command{
		cmd = proplists:get_value( <<"cmd">>, MsgData ),
		args = proplists:get_value( <<"args">>, MsgData, [] )
	},
	io:format( "Command: ~p~n", [ Command ] ),
	{ reply, { text, mochijson2:encode( { struct, [ {<<"status">>,<<"ok">>} ] } ) }, Req, C };
websocket_handle( _, Req, C ) ->
	{ok, Req, C}.




websocket_info( { mpd_now_playing, Playing }, Req, C ) ->
	Message = { struct, [
		{ <<"now_playing">>, { struct, Playing } } 
	] },
	{ reply, { text, mochijson2:encode( Message ) }, Req, C };
websocket_info( _, Req, C ) ->
	{ok, Req, C}.




websocket_terminate(_Reason, _Req, _C) ->
	ok.
