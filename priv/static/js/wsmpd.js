define( [ 'jquery' ], function( jquery ) {

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
	
	return {
		connect: function( host ) {
			websocket = new WebSocket( host );
	
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
		
		addid: function( songid ) {
			return sendCommand( 'addid', [ songid ] );
		},

		currentsong: function() {
			return sendCommand( 'currentsong', [] );
		},

		status: function() {
			return sendCommand( 'status', [] );
		},

		random: function( state ) {
			return sendCommand( 'random', [ state ] );
		},

		repeat: function( state ) {
			return sendCommand( 'repeat', [ state ] );
		},

		next: function() {
			return sendCommand( 'next', [] );
		},

		pause: function( state ) {
			return sendCommand( 'pause', [ state ] );
		},

		play: function( position ) {
			return sendCommand( 'play', [ position ] );
		},

		previous: function() {
			return sendCommand( 'previous', [] );
		},

		clear: function() {
			return sendCommand( 'clear', [] );
		},

		delete: function( Pos ) {
			return sendCommand( 'delete', [ Pos ] );
		},

		playlistinfo: function() {
			return sendCommand( 'playlistinfo', [] );
		},

		search: function( type, what ) {
			return sendCommand( 'search', [ type, new String( what ).toString() ] );
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

		outputs: function() {
			return sendCommand( 'outputs', [] );
		}
	};

} );
