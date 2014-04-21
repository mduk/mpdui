-module( mpdui ).

-export( [ start/0 ] ).

start() ->
	lists:map( fun( App ) ->
		application:start( App )
	end, [ sasl, ranch, cowlib, crypto, cowboy, mpdui ] ).
