-module( mpdui_command ).

-export( [ execute/3 ] ).

%% Status

execute( C, <<"currentsong">>, [] ) ->
	object( <<"currentsong">>, object( erlmpd:currentsong( C ) ) );

execute( C, <<"status">>, [] ) ->
	object( <<"status">>, object( erlmpd:status( C ) ) );

execute( C, <<"stats">>, [] ) ->
	object( <<"stats">>, object( erlmpd:stats( C ) ) );

%% Playback Options

execute( C, <<"consume">>, [ 1 ] ) ->
	ok( erlmpd:consume( C, true ) );
execute( C, <<"consume">>, [ 0 ] ) ->
	ok( erlmpd:consume( C, false ) );
execute( _, <<"consume">>, _ ) ->
	error_msg( <<"Consume argument must be an integer, 0 or 1">> );

execute( C, <<"crossfade">>, [ Seconds ] ) when is_integer( Seconds ) ->
	ok( erlmpd:crossfade( C, Seconds ) );
execute( _, <<"crossfade">>, _ ) ->
	error_msg( <<"Seconds argument must be an integer">> );

execute( _, <<"mixrampdb">>, _ ) ->
	unsupported();
execute( _, <<"mixrampdelay">>, _ ) ->
	unsupported();

execute( C, <<"random">>, [ 1 ] ) ->
	ok( erlmpd:random( C, true ) );
execute( C, <<"random">>, [ 0 ] ) ->
	ok( erlmpd:random( C, false ) );
execute( _, <<"random">>, _ ) ->
	error_msg( <<"State argument must be an integer, 0 or 1">> );

execute( C, <<"repeat">>, [ 1 ] ) ->
	ok( erlmpd:repeat( C, true ) );
execute( C, <<"repeat">>, [ 0 ] ) ->
	ok( erlmpd:repeat( C, false ) );
execute( _, <<"repeat">>, _ ) ->
	error_msg( <<"State argument must be an integer, 0 or 1">> );

execute( C, <<"setvol">>, [ Vol ] ) when is_integer( Vol ); Vol >= 0; Vol =< 100 ->
	ok( erlmpd:setvol( C, Vol ) );
execute( _, <<"setvol">>, [ 0 ] ) ->
	error_msg( <<"Volume argument must be an integer between 0 and 100 inclusive">> );

execute( C, <<"single">>, [ 1 ] ) ->
	ok( erlmpd:single( C, true ) );
execute( C, <<"single">>, [ 0 ] ) ->
	ok( erlmpd:single( C, false ) );
execute( _, <<"single">>, _ ) ->
	error_msg( <<"State argument must be an integer, 0 or 1">> );

%% Controlling Playback

execute( C, <<"next">>, [] ) ->
	ok( erlmpd:next( C ) );

execute( C, <<"pause">>, [ 1 ] ) ->
	ok( erlmpd:pause( C, true ) );
execute( C, <<"pause">>, [ 0 ] ) ->
	ok( erlmpd:pause( C, false ) );
execute( _, <<"pause">>, _ ) ->
	error_msg( <<"State argument must be an integer, 0 or 1">> );

execute( C, <<"play">>, [] ) ->
	ok( erlmpd:play( C ) );
execute( C, <<"play">>, [ Position ] ) when is_integer( Position ) ->
	ok( erlmpd:play( C, Position ) );
execute( _, <<"play">>, _ ) ->
	error_msg( <<"First argument must be an integer">> );

execute( C, <<"playid">>, [] ) ->
	ok( erlmpd:playid( C ) );
execute( C, <<"playid">>, [ Id ] ) when is_integer( Id ) ->
	ok( erlmpd:playid( C, Id ) );
execute( _, <<"playid">>, _ ) ->
	error_msg( <<"First argument must be an integer">> );

execute( C, <<"previous">>, [] ) ->
	ok( erlmpd:previous( C ) );

execute( C, <<"seek">>, [ Position, Time ] ) when is_integer( Position ) and is_integer( Time ) ->
	ok( erlmpd:seek( C, Position, Time ) );
execute( _, <<"seek">>, _ ) ->
	error_msg( <<"Position and Time arguments must be integers">> );

execute( C, <<"seekid">>, [ Id, Time ] ) when is_integer( Id ) and is_integer( Time ) ->
	ok( erlmpd:seekid( C, Id, Time ) );
execute( _, <<"seekid">>, _ ) ->
	error_msg( <<"Id and Time arguments must be integers">> );

execute( _, <<"seekcur">>, _ ) ->
	unsupported();

execute( C, <<"stop">>, [] ) ->
	ok( erlmpd:stop( C ) );

%% Current Playlist

execute( C, <<"add">>, [ Uri ] ) ->
	ok( erlmpd:add( C, binary_to_list( Uri ) ) );

execute( C, <<"addid">>, [ Uri ] ) ->
	object( <<"id">>, erlmpd:addid( C, binary_to_list( Uri ) ) );
execute( C, <<"addid">>, [ Uri, Position ] ) ->
	object( <<"id">>, erlmpd:addid( C, binary_to_list( Uri ), Position ) );

execute( C, <<"clear">>, [] ) ->
	ok( erlmpd:clear( C ) );

execute( C, <<"playlistinfo">>, [] ) ->
	Results = erlmpd:playlistinfo( C ),
	ResultsStruct = lists:map( fun object/1, Results ),
	object( <<"playlistinfo">>, ResultsStruct );

%% Stored Playlists

%% Music Database

execute( C, <<"search">>, [ Type, What ] ) ->
	Results = erlmpd:search( C, 
		binary_to_atom( Type, utf8 ), 
		binary_to_list( What ) 
	),
	ResultsStruct = lists:map( fun object/1, Results ),
	{ struct, [
		{ <<"search">>, [ Type, What ] },
		{ <<"results">>, ResultsStruct }
	] };

%% Stickers

%% Connection Settings

%% Audio Output Devices

execute( C, <<"disableoutput">>, [ Id ] ) when is_integer( Id ) ->
	ok( erlmpd:disableoutput( C, Id ) );
execute( _, <<"disableoutput">>, _ ) ->
	error_msg( <<"Id argument must be an integer">> );

execute( C, <<"enableoutput">>, [ Id ] ) when is_integer( Id ) ->
	ok( erlmpd:enableoutput( C, Id ) );
execute( _, <<"enableoutput">>, _ ) ->
	error_msg( <<"Id argument must be an integer">> );

execute( _, <<"toggleoutput">>, _ ) ->
	unsupported();

execute( C, <<"outputs">>, [] ) ->
	Results = erlmpd:outputs( C ),

	ResultsStruct = lists:map( fun( Result ) ->
		{ struct, Result }
	end, Results ),

	{ struct, [
		{ <<"outputs">>, ResultsStruct }
	] };

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

%% Private Functions

ok( ok ) -> { struct, [ { <<"ok">>, true } ] };
ok( _ ) -> { struct, [ { <<"ok">>, false } ] }.

object( Proplist ) when is_list( Proplist ) ->
	{ struct, Proplist };
object( _ ) ->
	{ struct, [] }.

object( Key, Proplist ) -> { struct, [
	{ Key, Proplist }
] }.

error_msg( Msg ) -> { struct, [
	{ <<"ok">>, false },
	{ <<"error">>, Msg }
] }.

unsupported() -> error_msg( <<"erlmpd doesn't seem to have that one... :(">> ).
