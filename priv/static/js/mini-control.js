define( function( require ) {
	'use strict';
	
	var defineComponent = require('flight/component'),
	    jquery = require('jquery'),
	    wsmpd = require('wsmpd');

	return defineComponent( miniControl );

	function miniControl() {
		
		this.after('initialize', function() {
			this.on( document, 'status', this.onStatus );
			
			this.on( '#minicontrol-previous', 'click', this.clickPrevious );
			this.on( '#minicontrol-playpause', 'click', this.clickPlayPause );
			this.on( '#minicontrol-next', 'click', this.clickNext );
		});

		this.currentState = 'pause';

		this.clickPrevious = function() {
			wsmpd.previous();
		};

		this.clickPlayPause = function() {
			switch ( this.currentState ) {
				case 'pause':
					wsmpd.pause( 0 );
					this.updatePlayPauseButton( 'play' );
					break;

				case 'play':
					wsmpd.pause( 1 );
					this.updatePlayPauseButton( 'pause' );
					break;
			}
			
		};

		this.clickNext = function() {
			wsmpd.next();
		};

		this.onStatus = function( e, status ) {
			this.updatePlayPauseButton( status.state );
		};
		
		this.updatePlayPauseButton = function( state ) {
			if ( status.state == this.currentState ) {
				return;
			}

			switch ( state ) {
				case 'play':
					this.$node.find('#minicontrol-playpause')
					    .html('<span class="glyphicon glyphicon-pause"></span>');
					break;
	
				case 'pause':
					this.$node.find('#minicontrol-playpause')
					    .html('<span class="glyphicon glyphicon-play"></span>');
					break;
			}

			this.currentState = state;
		};
	}

} );
