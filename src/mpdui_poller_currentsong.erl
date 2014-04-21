-module( mpdui_poller_currentsong ).

-export( [ start_link/2 ] ).

start_link( Pid, C ) ->
	spawn_link( fun() -> loop( Pid, C, [] ) end ).

loop( Pid, C, LastPoll ) ->
	Poll = erlmpd:currentsong( C ),
	NewLastPoll = notifyChange(Pid, LastPoll, Poll),
	timer:sleep( 1000 ),
	loop( Pid, C, NewLastPoll ).

notifyChange( _, LastPoll, LastPoll ) -> LastPoll;
notifyChange( Pid, _, Poll ) ->
	Pid ! { mpd_now_playing, Poll },
	Poll.