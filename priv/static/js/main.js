require.config( {
	paths: {
		"jquery"   : "/static/bower_components/jquery/dist/jquery.min",
		"wsmpd"    : "/static/js/wsmpd",
		"mustache" : "/static/bower_components/mustache/mustache",
		"bootstrap": "/static/bower_components/bootstrap/dist/js/bootstrap.min"
	},
	shim: {
		"bootstrap": {
			deps: [ "jquery" ],
			exports: "$"
		}
	},
    enforceDefine: true,
	waitSeconds: 10
} );

require( [ 'jquery', 'wsmpd', 'mustache', 'bootstrap' ], function( jquery, wsmpd, mustache ) {

	wsmpd.registerCurrentsongCallback( function( currentsong ) {
		var template = jquery('#tpl-currentsong').html();
		mustache.parse( template );
		jquery('#currentsong').html( mustache.render( template, {
			title: currentsong.Title,
			artist: currentsong.Artist,
			duration: currentsong.Time
		} ) );

		wsmpd.playlistinfo();
	} );

	wsmpd.registerStatusCallback( function( status ) {
		jquery('#currentsong-position').html( Math.round( status.time ).toString() );

		// Now Playing: Play/Pause button
		var playpause = jquery('#currentsong-playpause');
		switch ( status.state ) {
			case 'play':
				playpause.html('<span class="glyphicon glyphicon-pause"></span>');
				playpause.click( function() {
					wsmpd.pause( 1 );
				} );
				break;

			case 'pause':
				playpause.html('<span class="glyphicon glyphicon-play"></span>');
				playpause.click( function() {
					wsmpd.pause( 0 );
				} );
				break;
		}

		updateBtn( jquery('#currentsong-random'), status.random );
		updateBtn( jquery('#currentsong-repeat'), status.repeat );
	} );

	function updateBtn( $btn, state ) {
		if ( state ) {
			$btn.removeClass('btn-default').addClass('btn-primary');
		}
		else {
			$btn.addClass('btn-default').removeClass('btn-primary');
		}
	}

	wsmpd.registerCallback( function( message ) {

		// Received playlistinfo
		if ( typeof message.playlistinfo == 'object' ) {
			var template = jquery('#tpl-queue-row').html();
			mustache.parse( template );

			jquery('#queue').html('');
			message.playlistinfo.forEach( function( Elem ) {
				jquery('#queue').append(
					jquery( mustache.render( template, Elem ) )
				);
			} );
		}

	} );

	jquery('#currentsong-previous').click( function() {
		wsmpd.previous();
	} );

	jquery('#currentsong-next').click( function() {
		wsmpd.next();
	} );

	jquery('#currentsong-repeat').click( function( e ) {
		if ( jquery('#currentsong-repeat').hasClass( 'btn-primary' ) ) {
			wsmpd.repeat( 0 );
		}
		else {
			wsmpd.repeat( 1 );
		}
	} );

	jquery('#currentsong-random').click( function() {
		if ( jquery('#currentsong-random').hasClass( 'btn-primary' ) ) {
			wsmpd.random( 0 );
		}
		else {
			wsmpd.random( 1 );
		}
	} );

	// Status

	jquery('#cmd-currentsong').click( function() {
		wsmpd.currentsong();
	} );

	jquery('#cmd-status').click( function() {
		wsmpd.status();
	} );

	jquery('#cmd-stats').click( function() {
		wsmpd.stats();
	} );

	// Playback Options

	jquery('#cmd-consume').click( function() {
		wsmpd.consume(
			parseInt( jquery("#consume_state_txt").val() )
		);
	} );

	jquery('#cmd-crossfade').click( function() {
		wsmpd.crossfade(
			parseInt( jquery("#crossfade_seconds_txt").val() )
		);
	} );

	jquery('#cmd-random').click( function() {
		wsmpd.random(
			parseInt( jquery("#random_state_txt").val() )
		);
	} );

	jquery('#cmd-repeat').click( function() {
		wsmpd.repeat(
			parseInt( jquery("#repeat_state_txt").val() )
		);
	} );

	jquery('#cmd-setvol').click( function() {
		wsmpd.setvol(
			parseInt( jquery("#setvol_vol_txt").val() )
		);
	} );

	jquery('#cmd-single').click( function() {
		wsmpd.single(
			parseInt( jquery("#single_state_txt").val() )
		);
	} );

	// Controlling Playback

	jquery('#cmd-previous').click( function() {
		wsmpd.previous();
	} );

	jquery('#cmd-play').click( function() {
		wsmpd.play(
			parseInt( jquery('#cmd-play-position').val() )
		);
	} );

	jquery('#cmd-pause').click( function() {
		wsmpd.pause( 1 );
	} );

	jquery('#cmd-unpause').click( function() {
		wsmpd.pause( 0 );
	} );

	jquery('#cmd-stop').click( function() {
		wsmpd.stop();
	} );

	jquery('#cmd-next').click( function() {
		wsmpd.next();
	} );

	jquery('#cmd-playid').click( function() {
		wsmpd.playid(
			parseInt( jquery("#playid_txt").val() )
		);
	} );

	jquery('#cmd-seek').click( function() {
		wsmpd.seek(
			parseInt( jquery("#seek_position_txt").val() ),
			parseInt( jquery("#seek_time_txt").val() )
		);
	} );

	jquery('#cmd-seekid').click( function() {
		wsmpd.seekid(
			parseInt( jquery("#seekid_id_txt").val() ),
			parseInt( jquery("#seekid_time_txt").val() )
		);
	} );

	jquery('#cmd-seekcur').click( function() {
		wsmpd.seekcur(
			parseInt( jquery("#seekcur_time_txt").val() )
		);
	} );

	// Current Playlist

	jquery('#cmd-playlist').click( function() {
		wsmpd.playlist();
	} );

	jquery('#cmd-playlistinfo').click( function() {
		wsmpd.playlistinfo();
	} );

	jquery('#cmd-clear').click( function() {
		wsmpd.clear();
	} );

	// Database

	jquery('#cmd-search').click( function() {
		wsmpd.search(
			jquery("#search_type").val(),
			jquery("#search_txt").val()
		);
	} );

	// Output Device

	jquery('#cmd-disableoutput').click( function() {
		wsmpd.disableoutput(
			parseInt( jquery("#disableoutput_id_txt").val() )
		);
	} );

	jquery('#cmd-enableoutput').click( function() {
		wsmpd.enableoutput(
			parseInt( jquery("#enableoutput_id_txt").val() )
		);
	} );

	jquery('#cmd-toggleoutput').click( function() {
		wsmpd.toggleoutput(
			parseInt( jquery("#toggleoutput_id_txt").val() )
		);
	} );

	jquery(document).ready( function() {
		wsmpd.connect();
	} );
});
