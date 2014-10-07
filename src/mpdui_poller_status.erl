-module( mpdui_poller_status ).

-export( [ start_link/3 ] ).

start_link( Pid, C, Interval ) ->
	spawn_link( fun() -> loop( Pid, C, Interval, [] ) end ).

loop( Pid, C, Interval, LastPoll ) ->
	Poll = case erlmpd:status( C ) of
		{ error, Error } -> throw( { erlmpd_error, Error } );
		Else -> Else
	end,
	NewLastPoll = notifyChange(Pid, LastPoll, Poll),
	timer:sleep( Interval ),
	loop( Pid, C, Interval, NewLastPoll ).

notifyChange( _, LastPoll, LastPoll ) -> LastPoll;
notifyChange( Pid, LastPoll, ThisPoll ) ->
	Changes = find_changes( LastPoll, ThisPoll ),
	Pid ! { mpd_status, Changes },
	ThisPoll.

find_changes( ListOne, ListTwo ) ->
	lists:foldl( fun( { K, V }, Acc ) ->
		Diff = case proplists:get_value( K, ListOne ) of
			V -> []; % No difference
			_ -> [ { K, V } ] % One difference
		end,
		lists:append( Diff, Acc )
	end, [], ListTwo ).
