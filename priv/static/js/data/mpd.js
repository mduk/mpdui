define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component');

	return defineComponent( mpd );

	function mpd() {

		var websocket;

		function sendCommand( command, args ) {
			websocket.send( JSON.stringify( {
				cmd: command,
				args: args
			} ) );
		};

		function getEventName( message ) {
			var cmdName = function( message ) {
				return message.command.cmd;
			};

			var cmdAndFirstArg = function( message ) {
				return message.command.cmd + '-' + message.command.args[0];
			};

			switch ( message.command.cmd ) {
				case 'list': return cmdAndFirstArg( message );
				default: return cmdName( message );
			}
		};

		function bool2int( bool ) {
			return bool ? 1 : 0;
		};

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
			sendCommand( 'search', [ d.type, new String( d.what ).toString() ] );
			this.trigger( document, 'change-tab', { to: 'search' } ); // no
		};

		this.onRequestPlaylistinfo = function( e, d ) {
			sendCommand( 'playlistinfo', [] );
		};

		this.onRequestClear = function( e, d ) {
			sendCommand( 'clear', [] );
			sendCommand( 'playlistinfo', [] );
		};

		this.onRequestFindadd = function( e, d ) {
			sendCommand( 'findadd', [ d.type, new String( d.what ).toString() ] );
			sendCommand( 'playlistinfo', [] );
		};

		this.onRequestList = function( e, d ) {
			sendCommand( 'list', [ d.type ] );
		};

		this.onRequestPrevious = function( e, d ) {
			sendCommand( 'previous', [] );
		};

		this.onRequestNext = function( e, d ) {
			sendCommand( 'next', [] );
		};

		this.onRequestOutputs = function( e, d ) {
			sendCommand( 'outputs', [] );
		};

		this.onRequestPlay = function( e, d ) {
			sendCommand( 'play', [ d.pos ] );
		};

		this.onRequestDelete = function( e, d ) {
			sendCommand( 'delete', [ d.pos ] );
			sendCommand( 'playlistinfo', [] );
		};

		this.onRequestRandom = function( e, d ) {
			sendCommand( 'random', [ bool2int( d.state ) ] );
		};

		this.onRequestRepeat = function( e, d ) {
			sendCommand( 'repeat', [ bool2int( d.state ) ] );
		};

		this.onRequestPause = function( e, d ) {
			sendCommand( 'pause', [ bool2int( d.state ) ] );
		};

		this.onRequestAddid = function( e, d ) {
			sendCommand( 'addid', [ d.id ] );
			sendCommand( 'playlistinfo', [] );
		};

		this.onRequestConsume = function( e, d ) {
			sendCommand( 'consume', [ bool2int( d.state ) ] );
		};

		this.onRequestConnect = function( e, d ) {
			websocket = new WebSocket( d.host );

			var component = this;

			websocket.onopen = function() {
				component.trigger( document, 'websocket-connect' );
			};

			websocket.onclose = function() {
				component.trigger( document, 'websocket-disconnect' );
			};

			websocket.onmessage = function( e ) {
				var message = JSON.parse( e.data );

				if ( typeof message.status == 'object' ) {
					component.trigger( document, 'status', message.status );
					return;
				}

				if ( typeof message.currentsong == 'object' ) {
					component.trigger( document, 'currentsong', message.currentsong );
					return;
				}

				component.trigger( document, getEventName( message ), message );
			};
		};

		this.onWebsocketConnect = function() {
			this.trigger( document, 'request-playlistinfo' );
		};

	}

} );
