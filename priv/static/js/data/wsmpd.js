define( function( require ) {
	var defineComponent = require('flight/component'),
	    wsmpd = require('wsmpd');

	return defineComponent( mpd );

	var websocket;

	function mpd() {

		this.after( 'initialize', function() {
			this.on( document, 'request-search', function( e, d ) {
				wsmpd.search( d.type, d.what );
				this.trigger( document, 'change-tab', { to: 'search' } );
			} );

			this.on( document, 'request-playlistinfo', function() {
				wsmpd.playlistinfo();
			} );

			this.on( document, 'request-clear', function() {
				wsmpd.clear();
				wsmpd.playlistinfo();
			} );

			this.on( document, 'request-findadd', function( e, d ) {
				wsmpd.findadd( d.type, d.what );
				wsmpd.playlistinfo();
			} );

			this.on( document, 'request-list', function( e, d ) {
				wsmpd.list( d.type );
			} );

			this.on( document, 'request-previous', function( e, d ) {
				wsmpd.previous();
			} );

			this.on( document, 'request-next', function( e, d ) {
				wsmpd.next();
			} );

			this.on( document, 'request-outputs', function( e, d ) {
				wsmpd.outputs();
			} );

			this.on( document, 'request-play', function( e, d ) {
				wsmpd.play( d.pos );
			} );

			this.on( document, 'request-delete', function( e, d ) {
				wsmpd.delete( d.pos );
				wsmpd.playlistinfo();
			} );

			this.on( document, 'request-random', function( e, d ) {
				wsmpd.random( d.state );
			} );

			this.on( document, 'request-repeat', function( e, d ) {
				wsmpd.repeat( d.state );
			} );

			this.on( document, 'request-pause', function( e, d ) {
				wsmpd.pause( d.state );
			} );

			this.on( document, 'request-addid', function( e, d ) {
				wsmpd.addid( d.id );
				wsmpd.playlistinfo();
			} );

			this.on( document, 'request-consume', function( e, d ) {
				wsmpd.consume( d.state );
			} );

			// Connect to an MPD host
			this.on( document, 'request-connect', function( e, d ) {
				wsmpd.connect( d.host );
			} );

			// Request the current playlistinfo on connection
			this.on( document, 'websocket-connect', function() {
				wsmpd.playlistinfo();
			} );
		} );

	}

} );
