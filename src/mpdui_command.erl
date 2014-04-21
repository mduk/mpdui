-module( mpdui_command ).

-export( [ execute/3 ] ).

execute( C, <<"search">>, [ Type, What ] ) ->
	Results = erlmpd:search( C, binary_to_atom( Type, utf8 ), binary_to_list( What ) ),
	
	ResultsStruct = lists:map( fun( Result ) ->
		{ struct, Result }
	end, Results ),
	
	{ struct, [
		{ <<"search">>, [ Type, What ] },
		{ <<"results">>, ResultsStruct }
	] };
execute( C, <<"playlist">>, [] ) ->
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
execute( C, <<"add">>, [ Uri ] ) ->
	ok = erlmpd:add( C, binary_to_list( Uri ) ),
	{ struct, [ { <<"ok">>, true } ] };
execute( C, <<"addid">>, [ Uri ] ) ->
	SongId = erlmpd:addid( C, Uri ),
	{ struct, [
		{ <<"id">>, SongId }
	] };
execute( C, <<"addid">>, [ Uri, Position ] ) ->
	SongId = erlmpd:addid( C, Uri, Position ),
	{ struct, [
		{ <<"id">>, SongId }
	] };
execute( C, <<"clear">>, [] ) ->
	ok = erlmpd:clear( C ),
	{ struct, [ { <<"ok">>, true } ] };
execute( _Connection, Command, Args ) ->
	io:format(
		"*** Unsupported Command~n"
		"    Command: ~p~n"
		"    Args: ~p~n", [ Command, Args ] ),
	{ struct, [
		{ <<"error">>, <<"Unsupported command">> },
		{ <<"cmd">>, Command },
		{ <<"args">>, Args }
	] }.