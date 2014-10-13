-module( mpdui_command ).

-define( bin( A ), is_binary( A ) ).
-define( int( A ), is_integer( A ) ).

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

execute( C, <<"setvol">>, [ Vol ] )
when ?int( Vol ); Vol >= 0; Vol =< 100 ->
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
execute( C, <<"play">>, [ Position ] ) when ?int( Position ) ->
	ok( erlmpd:play( C, Position ) );
execute( _, <<"play">>, _ ) ->
	error_msg( <<"First argument must be an integer">> );

execute( C, <<"playid">>, [] ) ->
	ok( erlmpd:playid( C ) );
execute( C, <<"playid">>, [ Id ] ) when ?int( Id ) ->
	ok( erlmpd:playid( C, Id ) );
execute( _, <<"playid">>, _ ) ->
	error_msg( <<"First argument must be an integer">> );

execute( C, <<"previous">>, [] ) ->
	ok( erlmpd:previous( C ) );

execute( C, <<"seek">>, [ Position, Time ] )
when ?int( Position ) and ?int( Time ) ->
	ok( erlmpd:seek( C, Position, Time ) );
execute( _, <<"seek">>, _ ) ->
	error_msg( <<"Position and Time arguments must be integers">> );

execute( C, <<"seekid">>, [ Id, Time ] ) when ?int( Id ) and ?int( Time ) ->
	ok( erlmpd:seekid( C, Id, Time ) );
execute( _, <<"seekid">>, _ ) ->
	error_msg( <<"Id and Time arguments must be integers">> );

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

execute( C, <<"delete">>, [ Pos ] ) when ?int( Pos ) ->
	ok( erlmpd:delete( C, Pos ) );

execute( C, <<"playlistinfo">>, [] ) ->
	lists:map( fun object/1, erlmpd:playlistinfo( C ) );

%% Music Database

execute( C, <<"search">>, [ Type, What ] )
when ?bin( Type ), ?bin( What ) ->
	lists:map( fun object/1, erlmpd:search( C,
		binary_to_atom( Type, utf8 ), 
		binary_to_list( What ) 
	) );

execute( C, <<"list">>, [ Type ] ) when ?bin( Type ) ->
	Map = fun( Elem ) -> object( Type, Elem ) end,
	lists:map( Map, erlmpd:list( C, binary_to_atom( Type, utf8 ) ) );

execute( C, <<"list">>, [ Type, Artist ] ) when ?bin( Type ), ?bin( Artist ) ->
	Map = fun( Elem ) -> object( Type, Elem ) end,
	lists:map( Map, erlmpd:list( C,
		binary_to_atom( Type, utf8 ),
		binary_to_list( Artist )
	) );

execute( C, <<"list">>, [ ListType, FilterType, FilterValue ] )
when ?bin( ListType ), ?bin( FilterType ), ?bin( FilterValue ) ->
	Map = fun( Elem ) -> object( ListType, Elem ) end,
	lists:map( Map, erlmpd:list( C,
		binary_to_atom( ListType, utf8 ),
		binary_to_atom( FilterType, utf8 ),
		binary_to_list( FilterValue )
	) );

execute( C, <<"find">>, [ Type, What ] )
when ?bin( Type ), ?bin( What ) ->
	Map = fun( Elem ) -> object( Elem ) end,
	R = erlmpd:find( C,
		binary_to_atom( Type, utf8 ),
		binary_to_list( What )
	),
	io:format("Found: ~p~n",[R]),
	lists:map( Map, R );

execute( C, <<"findadd">>, [ Type, What ] )
when ?bin( Type ), ?bin( What ) ->
	ok( erlmpd:findadd( C,
		binary_to_atom( Type, utf8 ),
		binary_to_list( What )
	) );

%% Stickers

%% Connection Settings

%% Audio Output Devices

execute( C, <<"disableoutput">>, [ Id ] ) when ?int( Id ) ->
	ok( erlmpd:disableoutput( C, Id ) );
execute( _, <<"disableoutput">>, _ ) ->
	error_msg( <<"Id argument must be an integer">> );

execute( C, <<"enableoutput">>, [ Id ] ) when ?int( Id ) ->
	ok( erlmpd:enableoutput( C, Id ) );
execute( _, <<"enableoutput">>, _ ) ->
	error_msg( <<"Id argument must be an integer">> );

execute( C, <<"outputs">>, [] ) ->
	lists:map( fun object/1, erlmpd:outputs( C ) );

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
object( Invalid ) ->
	error_logger:warning_msg( "Not a list! ~p~n", [ Invalid ] ),
	{ struct, [] }.

object( Key, Val ) -> { struct, [
	{ Key, Val }
] }.

error_msg( Msg ) -> { struct, [
	{ <<"ok">>, false },
	{ <<"error">>, Msg }
] }.
