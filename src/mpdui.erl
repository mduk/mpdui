-module( mpdui ).

-export( [ start/0 ] ).

start() ->
	lists:map( fun( App ) ->
		application:start( App )
	end, [ sasl, ranch, cowlib, crypto, cowboy ] ),

	Dispatch = cowboy_router:compile([
		{ '_', [
			{ "/", cowboy_static, { file, "./priv/index.html" } },
			{ "/websocket", mpdui_websocket, [] },
			{ "/static/[...]", cowboy_static, { dir, "./priv/static" } }
		] }
	] ),

	cowboy:start_http( http, 100, [ { port, 45001 } ], [ { env, [ { dispatch, Dispatch } ]  } ] ).
