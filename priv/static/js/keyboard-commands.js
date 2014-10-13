define( function( require ) {
	var defineComponent = require('flight/component');
	return defineComponent( keyboardCommands );

	function keyboardCommands() {

		this.after( 'initialize', function() {
			this.on( document, 'keypress', this.keypressDocument );
			this.on( document, 'keyup', this.keyupDocument );
		} );

		this.keypressDocument = function( e ) {

			if ( e.target != document.body ) {
				return;
			}

			switch( String.fromCharCode( e.charCode ) ) {
				case 's':
				case '/': this.trigger( document, 'change-tab', { to: 'search' } ); break;
				case 'n': this.trigger( document, 'change-tab', { to: 'nowplaying' } ); break;
				case 'b': this.trigger( document, 'change-tab', { to: 'browse' } ); break;
			}
		};

		this.keyupDocument = function( e ) {
			if ( e.keyCode == 27 ) {
				console.log('escape');
			}
		};

	}
} );
