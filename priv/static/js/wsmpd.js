define( [ 'jquery' ], function( jquery ) {

	var websocket,
	    currentsongCallback,
	    statusCallback,
	    generalCallback;
	
	function send( txt ) {
		if ( websocket.readyState == websocket.OPEN ) {
			websocket.send(txt);
		}
	}

	function sendCommand( command, arguments ) {
		send( JSON.stringify( {
			cmd: command,
			args: arguments
		} ) );
	}
	
	return {
		connect: function() {
			websocket = new WebSocket("ws://" + window.location.host + "/websocket");
	
			websocket.onopen = function(evt) {};
	
			websocket.onclose = function(evt) {};
	
			websocket.onmessage = function(evt) {
				var message = JSON.parse( evt.data );

				var statusEnabled = ( typeof statusCallback == 'function' );
				var generalEnabled = ( typeof generalCallback == 'function' );

				var isCurrentSong = ( typeof message.currentsong == 'object' );
				var isStatus = ( typeof message.status == 'object');

				if ( statusEnabled && isStatus ) {
					statusCallback( message.status );
				}
				else if ( generalEnabled ) {
					generalCallback( message );
				}

				if ( isStatus ) {
					$(document).trigger('status', message.status);
				}

				if ( isCurrentSong ) {
					$(document).trigger('currentsong', message.currentsong);
				}
			};
	
			websocket.onerror = function(evt) {};
		},
		
		disconnect: function() {
			websocket.close();
		},
		
		isConnected: function() {
			if ( websocket.readyState == websocket.OPEN ) {
				return true;
			} else {
				return false;
			}
		},

		registerCurrentsongCallback: function( callback ) {
			currentsongCallback = callback;
		},

		registerStatusCallback: function( callback ) {
			statusCallback = callback;
		},
		
		registerCallback: function( callback ) {
			generalCallback = callback;
		},

		addid: function( songid ) {
			return sendCommand( 'addid', [ songid ] );
		},

		// Status

		currentsong: function() {
			return sendCommand( 'currentsong', [] );
		},

		status: function() {
			return sendCommand( 'status', [] );
		},

		stats: function() {
			return sendCommand( 'stats', [] );
		},

		// Playback Options

		consume: function( state ) {
			return sendCommand( 'consume', [ state ] );
		},

		crossfade: function() {
			return sendCommand( 'crossfade', [] );
		},

		mixrampdb: function( decibels ) {
			return sendCommand( 'mixrampdb', [ decibels ] );
		},

		mixrampdeley: function( seconds ) {
			return sendCommand( 'mixrampdeley', [ seconds ] );
		},

		random: function( state ) {
			return sendCommand( 'random', [ state ] );
		},

		repeat: function( state ) {
			return sendCommand( 'repeat', [ state ] );
		},

		setvol: function( volume ) {
			return sendCommand( 'setvol', [ volume ] );
		},

		single: function( state ) {
			return sendCommand( 'single', [ state ] );
		},

		// Controlling Playback

		next: function() {
			return sendCommand( 'next', [] );
		},

		pause: function( state ) {
			return sendCommand( 'pause', [ state ] );
		},

		play: function( position ) {
			return sendCommand( 'play', [ position ] );
		},

		playid: function( id ) {
			return sendCommand( 'playid', [ id ] );
		},

		previous: function() {
			return sendCommand( 'previous', [] );
		},

		seek: function( position, time ) {
			return sendCommand( 'seek', [ position, time ] );
		},

		seekid: function( id, time ) {
			return sendCommand( 'seekid', [ id, time ] );
		},

		seekcur: function( time ) {
			return sendCommand( 'seekcur', [ time ] );
		},

		stop: function() {
			return sendCommand( 'stop', [] );
		},

		// Current Playlist

		clear: function() {
			return sendCommand( 'clear', [] );
		},

		playlistinfo: function() {
			return sendCommand( 'playlistinfo', [] );
		},

		// Database

		search: function( type, what ) {
			return sendCommand( 'search', [ type, what ] );
		},

		list: function( type, artist ) {
			var args = [];
			if ( typeof artist == 'undefined' ) {
				args = [ type ];
			}
			else {
				args = [ type, artist ];
			}
			return sendCommand( 'list', args );
		},

		// Output Devices

		disableoutput: function( id ) {
			return sendCommand( 'disableoutput', [ id ] );
		},

		enableoutput: function( id ) {
			return sendCommand( 'enableoutput', [ id ] );
		},

		toggleoutput: function( id ) {
			return sendCommand( 'toggleoutput', [ id ] );
		},

		outputs: function() {
			return sendCommand( 'outputs', [] );
		}
	};

} );
