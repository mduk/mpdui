define( [ 'jquery' ], function( jquery ) {

	var websocket;
	
	function send( txt ) {
		if ( websocket.readyState == websocket.OPEN ) {
			websocket.send(txt);
		}
	};

	function sendCommand( command, arguments ) {
		send( JSON.stringify( {
			cmd: command,
			args: arguments
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
	
	return {
		connect: function() {
			websocket = new WebSocket("ws://" + window.location.host + "/websocket");
	
			websocket.onopen = function(evt) {};
	
			websocket.onclose = function(evt) {};
	
			websocket.onmessage = function(evt) {
				var message = JSON.parse( evt.data );
				
				if ( typeof message.status == 'object') {
					jquery(document).trigger('status', message.status);
					return;
				}

				if ( typeof message.currentsong == 'object' ) {
					jquery(document).trigger('currentsong', message.currentsong);
					return;
				}

				jquery(document).trigger( getEventName( message ), message );
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

		findadd: function( type, what ) {
			return sendCommand( 'findadd', [ type, what ] );
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
