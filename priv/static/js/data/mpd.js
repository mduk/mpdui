define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    wsmpd = require('wsmpd');

	return defineComponent( mpd );

	var websocket;

	function mpd() {

		this.after( 'initialize', function() {
			this.on( document, 'request-search', this.onRequestSearch );
			this.on( document, 'request-playlistinfo', this.onRequestPlaylistinfo );
			this.on( document, 'request-clear', this.onRequestClear );
			this.on( document, 'request-findadd', this.onRequestFindadd );
			this.on( document, 'request-list', this.onRequestList );
			this.on( document, 'request-previous', this.onRequestPrevious );
			this.on( document, 'request-next', this.onRequestNext );
			this.on( document, 'request-outputs', this.onRequestOutputs );
			this.on( document, 'request-play', this.onRequestPlay );
			this.on( document, 'request-delete', this.onRequestDelete );
			this.on( document, 'request-random', this.onRequestRandom );
			this.on( document, 'request-repeat', this.onRequestRepeat );
			this.on( document, 'request-pause', this.onRequestPause );
			this.on( document, 'request-addid', this.onRequestAddid );
			this.on( document, 'request-consume', this.onRequestConsume );
			this.on( document, 'request-connect', this.onRequestConnect );
			this.on( document, 'websocket-connect', this.onWebsocketConnect );
		} );

		this.onRequestSearch = function( e, d ) {
			wsmpd.search( d.type, d.what );
			this.trigger( document, 'change-tab', { to: 'search' } );
		};

		this.onRequestPlaylistinfo = function( e, d ) {
			wsmpd.playlistinfo();
		};

		this.onRequestClear = function( e, d ) {
			wsmpd.clear();
			wsmpd.playlistinfo();
		};

		this.onRequestFindadd = function( e, d ) {
			wsmpd.findadd( d.type, d.what );
			wsmpd.playlistinfo();
		};

		this.onRequestList = function( e, d ) {
			wsmpd.list( d.type );
		};

		this.onRequestPrevious = function( e, d ) {
			wsmpd.previous();
		};

		this.onRequestNext = function( e, d ) {
			wsmpd.next();
		};

		this.onRequestOutputs = function( e, d ) {
			wsmpd.outputs();
		};

		this.onRequestPlay = function( e, d ) {
			wsmpd.play( d.pos );
		};

		this.onRequestDelete = function( e, d ) {
			wsmpd.delete( d.pos );
			wsmpd.playlistinfo();
		};

		this.onRequestRandom = function( e, d ) {
			wsmpd.random( d.state );
		};

		this.onRequestRepeat = function( e, d ) {
			wsmpd.repeat( d.state );
		};

		this.onRequestPause = function( e, d ) {
			wsmpd.pause( d.state );
		};

		this.onRequestAddid = function( e, d ) {
			wsmpd.addid( d.id );
			wsmpd.playlistinfo();
		};

		this.onRequestConsume = function( e, d ) {
			wsmpd.consume( d.state );
		};

		this.onRequestConnect = function( e, d ) {
			wsmpd.connect( d.host );
		};

		this.onWebsocketConnect = function() {
			wsmpd.playlistinfo();
		};

	}

} );
