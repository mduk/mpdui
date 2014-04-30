define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery'),
	    wsmpd = require('wsmpd');

	return defineComponent( nowPlaying,
		require('js/withTemplate')
	);

	function nowPlaying() {

		this.defaultAttrs( {
			withTemplate: 'now_playing'
		} );

		this.after('initialize', function() {
			this.on( document, 'status', this.onStatus );
			this.on( document, 'currentsong', this.onCurrentsong );

			this.render();
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
			this.render();
		};

		this.onCurrentsong = function( e, currentsong ) {
			this.title = currentsong.Title;
			this.artist = currentsong.Artist;
			this.album = currentsong.Album;
			this.duration = currentsong.Time;
			this.updatePositionPercent();
			this.render();
		};

		this.updatePositionPercent = function() {
			this.position_percent = ( 100 / this.duration ) * this.position;
		};
	}

} );
