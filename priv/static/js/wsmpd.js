define( [ 'jquery' ], function( jquery ) {

	var websocket,
	    currentsongCallback,
	    statusCallback;
	
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
			websocket = new WebSocket("ws://localhost:45001/websocket");
	
			websocket.onopen = function(evt) {
				jquery("#connected").fadeIn('slow');
				jquery("#content").fadeIn('slow');
			};
	
			websocket.onclose = function(evt) {
			};
	
			websocket.onmessage = function(evt) {
				var message = JSON.parse( evt.data );

				console.log( "recv", message );

				var statusEnabled = ( typeof statusCallback == 'function' );
				var currentsongEnabled = ( typeof statusCallback == 'function' );

				if ( statusEnabled && typeof message.status == 'object' ) {
					statusCallback( message.status );
				}

				if ( currentsongEnabled && typeof message.currentsong == 'object' ) {
					currentsongCallback( message.currentsong );
				}

				if ( currentsongEnabled && typeof message.now_playing == 'object' ) {
					currentsongCallback( message.now_playing );
				}
			};
	
			websocket.onerror = function(evt) {
			};
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

		playlist: function() {
			return sendCommand( 'playlist', [] );
		},

		playlistinfo: function() {
			return sendCommand( 'playlistinfo', [] );
		},

		// Database

		search: function( type, what ) {
			return sendCommand( 'search', [ type, what ] );
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
