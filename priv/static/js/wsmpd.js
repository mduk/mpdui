define( [ 'jquery' ], function( jquery ) {

	var websocket;
	
	function showScreen(txt) {
		jquery('#output').prepend('<p>' + txt + '</p>');
	};
	
	return {
		connect: function() {
			websocket = new WebSocket("ws://localhost:45001/websocket");
	
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
		},
		
		disconnect: function() {
			websocket.close();
		},
		
		send: function( txt ) {
			if ( websocket.readyState == websocket.OPEN ) {
				websocket.send(txt);
				showScreen('sending: ' + txt);
			} else {
				showScreen('websocket is not connected');
			}
		},
		
		sendCommand: function( command, argument ) {
			send( JSON.stringify( {
				cmd: command,
				args: arguments
			} ) );
		}
	};

} );
