define( function( require ) {
	var jquery = require( 'jquery' ),
		wsmpd = require( 'wsmpd' ),
		templates = require( 'js/templates' );
	
	require( 'bootstrap' );
	
	require( 'js/mini-control' ).attachTo( '#mini-control' );
	require( 'js/now-playing' ).attachTo( '#now-playing .jumbotron' );
	require( 'js/queue' ).attachTo( '#queue-container' );
	require( 'js/library' ).attachTo( '#library-container' );

	wsmpd.registerStatusCallback( function( status ) {
		jquery('#currentsong-position').html( Math.round( status.time ).toString() );

	} );

	wsmpd.registerCallback( function( message ) {

		if ( typeof message.playlistinfo == 'object' ) {
			// Not unpacking message message contents 'cause 
			// flightjs gets funky with non-object values apparently
			$(document).trigger( 'playlistinfo', message );
		}

		if ( typeof message.results == 'object' && typeof message.search == 'object' ) {
			$(document).trigger( 'search-results', message );
		}

		// Received artist list
		if ( typeof message.results == 'object' && typeof message.list == 'object' && message.list[0] == 'artist' ) {
			jquery('#artists-container').html(templates.list_panel.render( {
				title: "Artists",
				items: message.results.map( function( elem ) {
					return { title: elem.artist };
				} )
			} ));
		}

		// Received album list
		if ( typeof message.results == 'object' && typeof message.list == 'object' && message.list[0] == 'album' ) {
			
			jquery('#albums-container').html(templates.list_panel.render({
				title: "Albums",
				items: message.results.map( function( elem ) {
					return { title: elem.album };
				} )
			}));
		}

	} );

	jquery(document).on( 'currentsong', function(e, msg) {
		console.log( 'currentsong', msg );
		wsmpd.playlistinfo();
	} );

	jquery('#nav-search button[type=submit]').click( function( e ) {
		e.preventDefault();
		wsmpd.search(
			'album',
			jquery("#nav-search input[type=text]").val()
		);
	} );
	
	jquery('#nav-library').click( function() {
		wsmpd.list('artist');
		wsmpd.list('album');
	} );

	jquery('#cmd-list').click( function() {
		wsmpd.list(
			jquery("#cmd-list-type").val()
		);
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

	// return init function (main)
	return function() {
		jquery(document).ready( function() {
			wsmpd.connect();
		} );

		jquery('button').tooltip();
	};
} );
