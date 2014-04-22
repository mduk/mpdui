define( [ 'jquery' ], function( jquery ) {

	var websocket;
	
	function showScreen(txt) {
		jquery('#output').prepend('<p>' + txt + '</p>');
	};

	function send( txt ) {
		if ( websocket.readyState == websocket.OPEN ) {
			websocket.send(txt);
			showScreen('sending: ' + txt);
		} else {
			showScreen('websocket is not connected');
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
				showScreen('<span style="color: green;">CONNECTED</span>');
				jquery("#connected").fadeIn('slow');
				jquery("#content").fadeIn('slow');
			};
	
			websocket.onclose = function(evt) {
				showScreen('<span style="color: red;">DISCONNECTED</span>');
			};
	
			websocket.onmessage = function(evt) {
				var message = JSON.parse( evt.data );
				console.log( "wsmpd received", message );
			};
	
			websocket.onerror = function(evt) {
				showScreen('<span style="color: red;">ERROR: ' + evt.data + '</span>');
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
		
		sendCommand: function( command, args ) {
			sendCommand( command, args );
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

		consume: function() {
			return sendCommand( 'consume', [] );
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
