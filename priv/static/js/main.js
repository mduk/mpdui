require.config( {
	paths: {
		"jquery" : "/static/bower_components/jquery/dist/jquery.min",
		"wsmpd"  : "/static/js/wsmpd"
	},
	waitSeconds: 10
} );

require( [ 'jquery', 'wsmpd' ], function( jquery, wsmpd ) {

	jquery('#btn-toggleconnection').click( function() {
		if ( wsmpd.isConnected() ) {
			wsmpd.disconnect();
		} else {
			wsmpd.connect();
		}
	} );

	jquery('#cmd-sendtext').click( function() {
		wsmpd.send( jquery("#send_txt").val() );
	} );

	jquery('#cmd-currentsong').click( function() {
		wsmpd.sendCommand( 'currentsong', [] );
	} );

	jquery('#cmd-search').click( function() {
		wsmpd.sendCommand( 'search', [
			jquery("#search_type").val(),
			jquery("#search_txt").val()
		] );
	} );

	jquery('#cmd-previous').click( function() {
		wsmpd.sendCommand( 'previous', [] );
	} );

	jquery('#cmd-play').click( function() {
		wsmpd.sendCommand( 'play', [] );
	} );

	jquery('#cmd-pause').click( function() {
		wsmpd.sendCommand( 'pause', [ 1 ] );
	} );

	jquery('#cmd-unpause').click( function() {
		wsmpd.sendCommand( 'pause', [ 0 ] );
	} );

	jquery('#cmd-stop').click( function() {
		wsmpd.sendCommand( 'stop', [] );
	} );

	jquery('#cmd-next').click( function() {
		wsmpd.sendCommand( 'next', [] );
	} );

	jquery('#cmd-playid').click( function() {
		wsmpd.sendCommand( 'playid', [
			parseInt( jquery("#playid_txt").val() )
		] );
	} );

	jquery('#cmd-seek').click( function() {
		wsmpd.sendCommand( 'seek', [
			parseInt( jquery("#seek_position_txt").val() ),
			parseInt( jquery("#seek_time_txt").val() )
		] );
	} );

	jquery('#cmd-seekid').click( function() {
		wsmpd.sendCommand( 'seekid', [
			parseInt( jquery("#seekid_id_txt").val() ),
			parseInt( jquery("#seekid_time_txt").val() )
		] );
	} );

	jquery('#cmd-seekcur').click( function() {
		wsmpd.sendCommand( 'seekcur', [
			parseInt( jquery("#seekcur_time_txt").val() )
		] );
	} );

	jquery('#cmd-playlist').click( function() {
		wsmpd.sendCommand( 'playlist', [] );
	} );

	jquery('#cmd-playlistinfo').click( function() {
		wsmpd.sendCommand( 'playlistinfo', [] );
	} );

	jquery('#cmd-clear').click( function() {
		wsmpd.sendCommand( 'clear', [] );
	} );

	jquery('#cmd-disableoutput').click( function() {
		wsmpd.sendCommand( 'disableoutput', [
			parseInt( jquery("#disableoutput_id_txt").val() )
		] );
	} );

	jquery('#cmd-enableoutput').click( function() {
		wsmpd.sendCommand( 'enableoutput', [
			parseInt( jquery("#enableoutput_id_txt").val() )
		] );
	} );

	jquery('#cmd-toggleoutput').click( function() {
		wsmpd.sendCommand( 'toggleoutput', [
			parseInt( jquery("#toggleoutput_id_txt").val() )
		] );
	} );

	jquery('#cmd-consume').click( function() {
		wsmpd.sendCommand( 'consume', [
			parseInt( jquery("#consume_state_txt").val() )
		] );
	} );

	jquery('#cmd-crossfade').click( function() {
		wsmpd.sendCommand( 'crossfade', [
			parseInt( jquery("#crossfade_seconds_txt").val() )
		] );
	} );

	jquery('#cmd-random').click( function() {
		wsmpd.sendCommand( 'random', [
			parseInt( jquery("#random_state_txt").val() )
		] );
	} );

	jquery('#cmd-repeat').click( function() {
		wsmpd.sendCommand( 'repeat', [
			parseInt( jquery("#repeat_state_txt").val() )
		] );
	} );

	jquery('#cmd-setvol').click( function() {
		wsmpd.sendCommand( 'setvol', [
			parseInt( jquery("#setvol_vol_txt").val() )
		] );
	} );

	jquery('#cmd-single').click( function() {
		wsmpd.sendCommand( 'single', [
			parseInt( jquery("#single_state_txt").val() )
		] );
	} );

	jquery(document).ready( function() {
		wsmpd.connect();
		jquery("#connected").hide();
		jquery("#content").hide();
	} );
});
