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
			this.on( '#minicontrol-random', 'click', this.clickRandom );
		});

		this.playpauseState = 'pause';
		this.randomState = false;

		this.clickPrevious = function() {
			wsmpd.previous();
		};

		this.clickPlayPause = function() {
			switch ( this.playpauseState ) {
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

		this.clickRandom = function() {
			switch ( this.randomState ) {
				case true:
					wsmpd.random( 0 );
					this.updateRandomButton( false );
					break;

				case false:
					wsmpd.random( 1 );
					this.updateRandomButton( true );
					break;
			}
		};

		this.onStatus = function( e, status ) {
			this.updatePlayPauseButton( status.state );
			this.updateRandomButton( status.random );
		};
		
		this.updatePlayPauseButton = function( state ) {
			if ( state == this.playPauseState ) {
				return;
			}
			
			var btn = this.$node.find('#minicontrol-playpause');

			switch ( state ) {
				case 'play':
					btn.html('<span class="glyphicon glyphicon-pause"></span>');
					break;
	
				case 'pause':
					btn.html('<span class="glyphicon glyphicon-play"></span>');
					break;
			}

			this.playpauseState = state;
		};
		
		this.updateRandomButton = function( state ) {
			if ( state == this.randomState ) {
				return;
			}
			
			var btn = this.$node.find('#minicontrol-random');
			
			switch ( state ) {
				case false:
					btn.removeClass('btn-primary');
					break;

				case true:
					btn.addClass('btn-primary');
					break;
			}

			this.randomState = state;
		}
	}

} );