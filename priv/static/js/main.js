require.config( {
	paths: {
		"jquery": "/static/bower_components/jquery/dist/jquery.min",
	},
	waitSeconds: 10
} );

require( [ 'jquery' ], function( jquery ) {
	console.log("requirejs ready");

	var websocket;

	function connect() {
		wsHost = jquery("#server").val();
		websocket = new WebSocket(wsHost);

		websocket.onopen = function(evt) {
			showScreen('<span style="color: green;">CONNECTED </span>');
			jquery("#connected").fadeIn('slow');
			jquery("#content").fadeIn('slow');
		};

		websocket.onclose = function(evt) {
			showScreen('<span style="color: red;">DISCONNECTED </span>');
		};

		websocket.onmessage = function(evt) {
			console.log( JSON.parse( evt.data ) );
		};

		websocket.onerror = function(evt) {
			showScreen('<span style="color: red;">ERROR: ' + evt.data+ '</span>');
		};
	};

	function disconnect() {
		websocket.close();
	};

	function showScreen(txt) {
		jquery('#output').prepend('<p>' + txt + '</p>');
	};

	function send( txt ) {
		if(websocket.readyState == websocket.OPEN){
			websocket.send(txt);
			showScreen('sending: ' + txt);
		} else {
			showScreen('websocket is not connected');
		};
	};

	function sendCommand( command, arguments ) {
		send( JSON.stringify( {
			cmd: command,
			args: arguments
		} ) );
	};



	jquery('#btn-toggleconnection').click( function() {
		if(websocket.readyState == websocket.OPEN){
			disconnect();
		} else {
			connect();
		}
	} );



	jquery('#cmd-sendtext').click( function() {
		send( jquery("#send_txt").val() );
	} );

	jquery('#cmd-currentsong').click( function() {
		sendCommand( 'currentsong', [] );
	} );

	jquery('#cmd-search').click( function() {
		sendCommand( 'search', [
			jquery("#search_type").val(),
			jquery("#search_txt").val()
		] );
	} );

	jquery('#cmd-previous').click( function() {
		sendCommand( 'previous', [] );
	} );

	jquery('#cmd-play').click( function() {
		sendCommand( 'play', [] );
	} );

	jquery('#cmd-pause').click( function() {
		sendCommand( 'pause', [ 1 ] );
	} );

	jquery('#cmd-unpause').click( function() {
		sendCommand( 'pause', [ 0 ] );
	} );

	jquery('#cmd-stop').click( function() {
		sendCommand( 'stop', [] );
	} );

	jquery('#cmd-next').click( function() {
		sendCommand( 'next', [] );
	} );

	jquery('#cmd-playid').click( function() {
		sendCommand( 'playid', [
			parseInt( jquery("#playid_txt").val() )
		] );
	} );

	jquery('#cmd-seek').click( function() {
		sendCommand( 'seek', [
			parseInt( jquery("#seek_position_txt").val() ),
			parseInt( jquery("#seek_time_txt").val() )
		] );
	} );

	jquery('#cmd-seekid').click( function() {
		sendCommand( 'seekid', [
			parseInt( jquery("#seekid_id_txt").val() ),
			parseInt( jquery("#seekid_time_txt").val() )
		] );
	} );

	jquery('#cmd-seekcur').click( function() {
		sendCommand( 'seekcur', [
			parseInt( jquery("#seekcur_time_txt").val() )
		] );
	} );

	jquery('#cmd-playlist').click( function() {
		sendCommand( 'playlist', [] );
	} );

	jquery('#cmd-playlistinfo').click( function() {
		sendCommand( 'playlistinfo', [] );
	} );

	jquery('#cmd-clear').click( function() {
		sendCommand( 'clear', [] );
	} );

	jquery('#cmd-disableoutput').click( function() {
		sendCommand( 'disableoutput', [
			parseInt( jquery("#disableoutput_id_txt").val() )
		] );
	} );

	jquery('#cmd-enableoutput').click( function() {
		sendCommand( 'enableoutput', [
			parseInt( jquery("#enableoutput_id_txt").val() )
		] );
	} );

	jquery('#cmd-toggleoutput').click( function() {
		sendCommand( 'toggleoutput', [
			parseInt( jquery("#toggleoutput_id_txt").val() )
		] );
	} );

	jquery('#cmd-consume').click( function() {
		sendCommand( 'consume', [
			parseInt( jquery("#consume_state_txt").val() )
		] );
	} );

	jquery('#cmd-crossfade').click( function() {
		sendCommand( 'crossfade', [
			parseInt( jquery("#crossfade_seconds_txt").val() )
		] );
	} );

	jquery('#cmd-random').click( function() {
		sendCommand( 'random', [
			parseInt( jquery("#random_state_txt").val() )
		] );
	} );

	jquery('#cmd-repeat').click( function() {
		sendCommand( 'repeat', [
			parseInt( jquery("#repeat_state_txt").val() )
		] );
	} );

	jquery('#cmd-setvol').click( function() {
		sendCommand( 'setvol', [
			parseInt( jquery("#setvol_vol_txt").val() )
		] );
	} );

	jquery('#cmd-single').click( function() {
		sendCommand( 'single', [
			parseInt( jquery("#single_state_txt").val() )
		] );
	} );



	jquery(document).ready( function() {
		connect();
		jquery("#connected").hide();
		jquery("#content").hide();
	} );
});
