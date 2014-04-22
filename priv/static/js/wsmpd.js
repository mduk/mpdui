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
				showScreen('<span style="color: red;">ERROR: ' + evt.data+ '</span>');
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

		// Current Playlist
	};

} );
