define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery'),
	    templates = require('js/templates'),
	    wsmpd = require('wsmpd');

	return defineComponent( nowPlaying );

	function nowPlaying() {

		this.after('initialize', function() {
			this.on( document, 'status', this.onStatus );
			this.on( document, 'currentsong', this.onCurrentsong );

			this.renderNowPlaying();
		} );

		this.state = '';
		this.title = '';
		this.artist = '';
		this.album = '';
		this.duration = 0;
		this.position = 0;
		this.position_percent = 0;

		this.onStatus = function( e, status ) {
			this.state = status.state;
			this.position = status.time;
			this.updatePositionPercent();
			this.renderNowPlaying();
		};

		this.onCurrentsong = function( e, currentsong ) {
			this.title = currentsong.Title;
			this.artist = currentsong.Artist;
			this.album = currentsong.Album;
			this.duration = currentsong.Time;
			this.updatePositionPercent();
			this.renderNowPlaying();
		};

		this.updatePositionPercent = function() {
			this.position_percent = ( 100 / this.duration ) * this.position;
		};

		this.renderNowPlaying = function() {
			this.$node.html( templates.now_playing.render( this ) );
		};
	}

} );
