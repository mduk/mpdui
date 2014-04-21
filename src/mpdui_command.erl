-module( mpdui_command ).

-export( [ execute/3 ] ).

%% Status

%% Playback Options

%% Controlling Playback

execute( C, <<"next">>, [] ) ->
	ok( erlmpd:next( C ) );
execute( C, <<"pause">>, [ <<"true">> ] ) ->
	ok( erlmpd:pause( C, true ) );
execute( C, <<"pause">>, [ <<"false">> ] ) ->
	ok( erlmpd:pause( C, false ) );
execute( C, <<"play">>, [] ) ->
	ok( erlmpd:play( C ) );
execute( C, <<"play">>, [ Position ] ) when is_integer( Position ) ->
	ok( erlmpd:play( C, Position ) );
execute( _, <<"play">>, [ _ ] ) ->
	error_msg( <<"First argument must be an integer">> );
execute( C, <<"playid">>, [] ) ->
	ok( erlmpd:playid( C ) );
execute( C, <<"playid">>, [ Id ] ) when is_integer( Id ) ->
	ok( erlmpd:playid( C, Id ) );
execute( _, <<"playid">>, [ _ ] ) ->
	error_msg( <<"First argument must be an integer">> );
execute( C, <<"previous">>, [] ) ->
	ok( erlmpd:previous( C ) );
execute( C, <<"seek">>, [ Position, Time ] ) when is_integer( Position ) and is_integer( Time ) ->
	ok( erlmpd:seek( C, Position, Time ) );

%% Current Playlist

execute( C, <<"add">>, [ Uri ] ) ->
	ok( erlmpd:add( C, binary_to_list( Uri ) ) );
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
	ok( erlmpd:clear( C ) );
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
execute( C, <<"playlistinfo">>, [] ) ->
	Results = erlmpd:playlistinfo( C ),

	ResultsStruct = lists:map( fun( Result ) ->
		{ struct, Result }
	end, Results ),

	{ struct, [
		{ <<"playlistinfo">>, ResultsStruct }
	] };

%% Stored Playlists

%% Music Database

execute( C, <<"search">>, [ Type, What ] ) ->
	Results = erlmpd:search( C, 
		binary_to_atom( Type, utf8 ), 
		binary_to_list( What ) 
	),

	ResultsStruct = lists:map( fun( Result ) ->
		{ struct, Result }
	end, Results ),

	{ struct, [
		{ <<"search">>, [ Type, What ] },
		{ <<"results">>, ResultsStruct }
	] };

%% Stickers

%% Connection Settings

%% Audio Output Devices

%% Unknown command

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

ok( ok ) -> { struct, [ { <<"ok">>, true } ] };
ok( _ ) -> { struct, [ { <<"ok">>, false } ] }.

error_msg( Msg ) -> { struct, [
	{ <<"ok">>, false },
	{ <<"error">>, Msg }
] }.
