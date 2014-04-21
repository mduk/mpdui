-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export( [ init/3, websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3 ] ).

init( { tcp, http }, _Req, _Opts ) ->
	{ upgrade, protocol, cowboy_websocket }.

websocket_init( _, Req, _ ) ->
	{ ok, C } = erlmpd:connect(),
	WsHandler = self(),
	spawn_link( fun() ->
		mpdui_poller_currentsong:start_link( WsHandler, C, 1000 ),
		timer:sleep( 500 ),
		mpdui_poller_status:start_link( WsHandler, C, 1000 )
	end ),
	{ ok, Req, C }.

websocket_handle( { text, RawMsg }, Req, C ) ->
	{ struct, MsgData } = mochijson2:decode( RawMsg ),
	ReplyData = execute_command( C,
		proplists:get_value( <<"cmd">>, MsgData ),
		proplists:get_value( <<"args">>, MsgData, [] )
	),
	{ reply, { text, mochijson2:encode( ReplyData ) }, Req, C };
websocket_handle( _, Req, C ) ->
	{ok, Req, C}.




websocket_info( { mpd_now_playing, Playing }, Req, C ) ->
	Message = { struct, [
		{ <<"now_playing">>, { struct, Playing } } 
	] },
	{ reply, { text, mochijson2:encode( Message ) }, Req, C };
websocket_info( { mpd_status, Status }, Req, C ) ->
	Message = { struct, [
		{ <<"status">>, { struct, Status } } 
	] },
	{ reply, { text, mochijson2:encode( Message ) }, Req, C };
websocket_info( _, Req, C ) ->
	{ok, Req, C}.




websocket_terminate(_Reason, _Req, _C) ->
	ok.

execute_command( C, <<"search">>, [ Type, What ] ) ->
	Results = erlmpd:search( C, binary_to_atom( Type, utf8 ), binary_to_list( What ) ),
	
	ResultsStruct = lists:map( fun( Result ) ->
		{ struct, Result }
	end, Results ),
	
	{ struct, [
		{ <<"search">>, [ Type, What ] },
		{ <<"results">>, ResultsStruct }
	] };
execute_command( C, <<"playlist">>, [] ) ->
	Results = erlmpd:playlist( C ),
	
	ResultsStruct = lists:map( fun( Result ) ->
		[ _, [ _ | Path ] ] = string:tokens( binary_to_list( Result ), ":" ),
		{ struct, [
			{ <<"file">>, list_to_binary( Path ) }
		] }
	end, Results ),

	{ struct, [
		{ <<"playlist">>, ResultsStruct }
	] };
execute_command( _Connection, _Command, _Args ) ->
	throw( { error, unsupported_command } ).