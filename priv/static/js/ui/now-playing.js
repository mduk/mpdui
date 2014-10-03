define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery');

	return defineComponent( nowPlaying,
		require('mixin/template')
	);

	function nowPlaying() {

		this.defaultAttrs( {
			withTemplate: 'now_playing',

			artistSelector: 'h2 span',
			albumSelector: 'h2 small'
		} );

		this.after('initialize', function() {
			this.on( document, 'status', this.onStatus );
			this.on( document, 'currentsong', this.onCurrentsong );

			this.on( 'click', {
				'artistSelector': this.clickArtist,
				'albumSelector': this.clickAlbum,
			} );

			this.render();
		} );

		this.state = '';
		this.title = '';
		this.artist = '';
		this.album = '';
		this.duration = 0;
		this.position = 0;
		this.position_percent = 0;

		this.clickArtist = function( e, d ) {
			this.trigger( 'request-search', {
				type: 'artist',
				what: jquery( d.el ).html()
			} );
		};

		this.clickAlbum = function( e, d ) {
			this.trigger( 'request-search', {
				type: 'album',
				what: jquery( d.el ).html()
			} );
		};

		this.onStatus = function( e, status ) {
			this.state = status.state;
			this.position = status.time;
			this.updatePositionPercent();

			if ( this.state == 'stop' ) {
				this.$node.html('');
			}
			else {
				this.render();
			}
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
