-module( mpdui_websocket ).

-behaviour( cowboy_websocket_handler ).
-export( [ init/3, websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3 ] ).

init( { tcp, http }, _Req, _Opts ) ->
	{ upgrade, protocol, cowboy_websocket }.

websocket_init( _, Req, _ ) ->
	State = case erlmpd:connect() of
		{ ok, C } ->
			WsHandler = self(),
			spawn_link( fun() ->
				mpdui_poller_currentsong:start_link( WsHandler, C, 1000 ),
				timer:sleep( 500 ),
				mpdui_poller_status:start_link( WsHandler, C, 1000 )
			end ),
			C;

		Err = { error, _ } ->
			throw( Err )

	end,
	{ ok, Req, State }.

websocket_handle( { text, RawMsg }, Req, C ) ->
	{ struct, MsgData } = mochijson2:decode( RawMsg ),
	{ ok, C2 } = erlmpd:connect(),
	ReplyData = execute_command( C2,
		proplists:get_value( <<"cmd">>, MsgData ),
		proplists:get_value( <<"args">>, MsgData, [] )
	),
	erlmpd:close( C2 ),

	{ reply, { text, mochijson2:encode( ReplyData ) }, Req, C };
websocket_handle( _, Req, C ) ->
	{ ok, Req, C }.
websocket_info( { mpd_currentsong, Playing }, Req, C ) ->
	Message = { struct, [
		{ <<"currentsong">>, { struct, Playing } }
	] },
	{ reply, { text, mochijson2:encode( Message ) }, Req, C };
websocket_info( { mpd_status, Status }, Req, C ) ->
	Message = { struct, [
		{ <<"status">>, { struct, Status } } 
	] },
	{ reply, { text, mochijson2:encode( Message ) }, Req, C };
websocket_info( _, Req, C ) ->
	{ ok, Req, C }.

websocket_terminate( _Reason, _Req, _C ) ->
	ok.

execute_command( C, Command, Args ) ->
	{ struct, [
		{ <<"command">>, { struct, [
			{ <<"cmd">>, Command },
			{ <<"args">>, Args }
		] } },
		{ <<"result">>, mpdui_command:execute( C, Command, Args ) }
	] }.
