-module( mpdui_app ).

-behaviour( application ).
-export( [ start/2, stop/1 ] ).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start( _StartType, _StartArgs ) ->
	io:format("starting mpdui...~n" ),
	Dispatch = cowboy_router:compile([
		{ '_', [
			{ "/", cowboy_static, { file, "./priv/index.html" } },
			{ "/websocket", mpdui_websocket, [] },
			{ "/static/[...]", cowboy_static, { dir, "./priv/static" } }
		] }
	] ),

	{ ok, _ } = cowboy:start_http( http, 100, [ { port, 45001 } ], [ { env, [ { dispatch, Dispatch } ]  } ] ).

stop( _State ) ->
    ok.
