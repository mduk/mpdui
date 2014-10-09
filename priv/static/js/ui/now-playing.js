define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery');

	return defineComponent(
		require('mixin/template'),
		require('mixin/event-imprint'),
		nowPlaying
	);

	function nowPlaying() {

		this.defaultAttrs( {
			withTemplate: 'now_playing',

			imprintEvents: {
				'status': [ 'state', 'time' ],
				'currentsong': [ 'Title', 'Album', 'Artist', 'Time' ]
			},

			imprints: {
				status: {
					state: 'stop'
				},
				currentsong: {
					Title: '-',
					Album: '-',
					Artist: '-',
					Time: 0
				},
			},

			artistSelector: 'h2 span',
			albumSelector: 'h2 small',

			position_percent: 0
		} );

		this.after('initialize', function() {
			this.onImprint( {
				'status': this.imprintUpdate,
				'currentsong': this.imprintUpdate
			} );

			this.on( 'click', {
				'artistSelector': this.clickArtist,
				'albumSelector': this.clickAlbum,
			} );
		} );

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

		this.imprintUpdate = function( imprint ) {
			this.updatePositionPercent();
			this.render();
		};

		this.updatePositionPercent = function() {
			this.attr.position_percent =
				( 100 / this.attr.imprints.currentsong.Time )
				* this.attr.imprints.status.time;
		};

		this.render = function() {
			if ( this.attr.imprints.status.state == 'stop' ) {
				this.$node.html('');
			}
			else {
				this.$node.html(
					templates[this.attr.withTemplate].render(this.attr, templates)
				);
			}
		};
	}

} );
