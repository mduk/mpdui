define( function( require ) {
	var defineComponent = require('flight/component'),
	    wsmpd = require('wsmpd');

	return defineComponent( mpd );

	var websocket;

	function mpd() {

		this.after( 'initialize', function() {
			console.log( 'wsmpd initialised' );

			this.on( document, 'request-search', function( e, d ) {
				wsmpd.search( d.type, d.what );
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

			wsmpd.connect();
		} );

	}

} );