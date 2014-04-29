define( function( require ) {
	var jquery = require( 'jquery' ),
		wsmpd = require( 'wsmpd' ),
		templates = require( 'js/templates' );
	
	require( 'bootstrap' );
	
	require( 'js/mini-control' ).attachTo( '#mini-control' );
	require( 'js/now-playing' ).attachTo( '#now-playing .jumbotron' );
	require( 'js/queue' ).attachTo( '#queue-container' );
	require( 'js/search' ).attachTo( '#search-container' );
	require( 'js/artist-list' ).attachTo( '#artists-container' );
	require( 'js/album-list' ).attachTo( '#albums-container' );

	wsmpd.registerCallback( function( message ) {
		
		var command = message.command;
		var result = message.result;
		
		if ( typeof result.playlistinfo == 'object' ) {
			// Not unpacking message message contents 'cause 
			// flightjs gets funky with non-object values apparently
			$(document).trigger( 'playlistinfo', message.result );
		}

		if ( typeof result.results == 'object' && typeof result.search == 'object' ) {
			$(document).trigger( 'search-results', message.result );
		}

		// Received artist list
		if ( typeof result.results == 'object' && typeof result.list == 'object' && result.list[0] == 'artist' ) {
			$(document).trigger( 'artist-list', message.result );
		}

		// Received album list
		if ( typeof result.results == 'object' && typeof result.list == 'object' && result.list[0] == 'album' ) {
			$(document).trigger( 'album-list', message.result );
		}

	} );

	jquery(document).on( 'currentsong', function(e, msg) {
		wsmpd.playlistinfo();
	} );

	jquery('#nav-search-form button[type=submit]').click( function( e ) {
		e.preventDefault();
		wsmpd.search(
			'album',
			jquery("#nav-search-form input[type=text]").val()
		);
		jquery('#nav-library').click();
	} );
	
	jquery('#nav-artists').click( function() {
		wsmpd.list('artist');
	} );
	
	jquery('#nav-albums').click( function() {
		wsmpd.list('album');
	} );

	// Settings

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
