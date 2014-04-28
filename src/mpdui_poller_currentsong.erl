-module( mpdui_poller_currentsong ).

-export( [ start_link/3 ] ).

start_link( Pid, C, Interval ) ->
	spawn_link( fun() -> loop( Pid, C, Interval, [] ) end ).

loop( Pid, C, Interval, LastPoll ) ->
	Poll = erlmpd:currentsong( C ),
	NewLastPoll = notifyChange(Pid, LastPoll, Poll),
	timer:sleep( Interval ),
	loop( Pid, C, Interval, NewLastPoll ).

notifyChange( _, LastPoll, LastPoll ) -> LastPoll;
notifyChange( Pid, _, Poll ) ->
	Pid ! { mpd_currentsong, Poll },
	Poll.
