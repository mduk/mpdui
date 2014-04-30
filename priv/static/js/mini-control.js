define( function( require ) {
	'use strict';
	
	var defineComponent = require('flight/component'),
	    jquery = require('jquery'),
	    wsmpd = require('wsmpd');

	return defineComponent( miniControl,
		require('js/withTemplate')
	);

	function miniControl() {

		this.defaultAttrs( {
			withTemplate: 'mini_control'
		} );

		this.after('initialize', function() {
			this.on( document, 'status', this.onStatus );

			this.render();
			
			this.on( '#minicontrol-previous', 'click', this.clickPrevious );
			this.on( '#minicontrol-playpause', 'click', this.clickPlayPause );
			this.on( '#minicontrol-next', 'click', this.clickNext );
			this.on( '#minicontrol-random', 'click', this.clickRandom );
			this.on( '#minicontrol-repeat', 'click', this.clickRepeat );
		});

		this.playpauseState = 'pause';
		this.randomState = false;
		this.repeatState = false;

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

				case 'stop':
					wsmpd.play( 0 );
					this.updatePlayPauseButton( 'play' );
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

		this.clickRepeat = function() {
			switch ( this.repeatState ) {
				case true:
					wsmpd.repeat( 0 );
					this.updateRepeatButton( false );
					break;

				case false:
					wsmpd.repeat( 1 );
					this.updateRepeatButton( true );
					break;
			}
		};

		this.onStatus = function( e, status ) {
			this.updatePlayPauseButton( status.state );
			this.updateRandomButton( status.random );
			this.updateRepeatButton( status.repeat );
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
				case 'stop':
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
		};
		
		this.updateRepeatButton = function( state ) {
			if ( state == this.repeatState ) {
				return;
			}
			
			var btn = this.$node.find('#minicontrol-repeat');
			
			switch ( state ) {
				case false:
					btn.removeClass('btn-primary');
					break;

				case true:
					btn.addClass('btn-primary');
					break;
			}

			this.repeatState = state;
		};
		
		this.render = function() {
			this.$node.html( templates.mini_control.render( this ) );
		};
	}

} );
