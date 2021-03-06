define( function( require ) {
	'use strict';
	
	var defineComponent = require('flight/component'),
	    jquery = require('jquery');

	return defineComponent( miniControl,
		require('mixin/template')
	);

	function miniControl() {

		this.defaultAttrs( {
			withTemplate: 'mini_control',
			
			previousBtnSelector: '#minicontrol-previous',
			playpauseBtnSelector: '#minicontrol-playpause',
			nextBtnSelector: '#minicontrol-next',
			randomBtnSelector: '#minicontrol-random',
			repeatBtnSelector: '#minicontrol-repeat'
		} );

		this.after('initialize', function() {
			this.on( document, 'status', this.onStatus );

			this.render();

			this.on( 'click', {
				previousBtnSelector: this.clickPrevious,
				playpauseBtnSelector: this.clickPlayPause, 
				nextBtnSelector: this.clickNext,
				randomBtnSelector: this.clickRandom,
				repeatBtnSelector: this.clickRepeat
			} );

		});

		this.playpauseState = 'pause';
		this.randomState = false;
		this.repeatState = false;

		this.clickPrevious = function() {
			this.trigger( document, 'request-previous' );
		};

		this.clickPlayPause = function() {
			switch ( this.playpauseState ) {
				case 'pause':
					this.trigger( document, 'request-pause', { state: 0 } );
					this.updatePlayPauseButton( 'play' );
					break;

				case 'play':
					this.trigger( document, 'request-pause', { state: 1 } );
					this.updatePlayPauseButton( 'pause' );
					break;

				case 'stop':
					this.trigger( document, 'request-play', { pos: 0 } );
					this.updatePlayPauseButton( 'play' );
					break;
			}
			
		};

		this.clickNext = function() {
			this.trigger( document, 'request-next' );
		};

		this.clickRandom = function() {
			switch ( this.randomState ) {
				case true:
					this.trigger( document, 'request-random', { state: 0 } );
					this.updateRandomButton( false );
					break;

				case false:
					this.trigger( document, 'request-random', { state: 1 } );
					this.updateRandomButton( true );
					break;
			}
		};

		this.clickRepeat = function() {
			switch ( this.repeatState ) {
				case true:
					this.trigger( document, 'request-repeat', { state: 0 } );
					this.updateRepeatButton( false );
					break;

				case false:
					this.trigger( document, 'request-repeat', { state: 1 } );
					this.updateRepeatButton( true );
					break;
			}
		};

		this.onStatus = function( e, status ) {
			if ( typeof status.state != 'undefined' ) {
				this.updatePlayPauseButton( status.state );
			}

			if ( typeof status.random != 'undefined' ) {
				this.updateRandomButton( status.random );
			}

			if ( typeof status.repeat != 'undefined' ) {
				this.updateRepeatButton( status.repeat );
			}
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
	}

} );
