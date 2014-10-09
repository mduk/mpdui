define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery');

	return defineComponent(
		require('mixin/template'),
		require('mixin/event-imprint'),
		queue
	);

	function queue() {

		this.defaultAttrs( {
			withTemplate: 'queue',

			imprintEvents: {
				status: [ 'consume' ],
				currentsong: [ 'Pos' ],
				playlistinfo: [ 'result' ]
			},

			imprints: {
				status: { consume: false },
				currentsong: { Pos: 0 },
				playlistinfo: { result: [] }
			},

			clearButtonSelector: 'button.clear-queue',
			consumeButtonSelector: 'button.consume-queue',
			removeButtonSelector: 'button.remove-from-queue',
			playNowButtonSelector: 'button.play-now',
			pauseButtonSelector: 'button.pause',
		} );

		this.after('initialize', function() {
			this.onImprint( 'currentsong', this.imprintCurrentsong );
			this.onImprint( 'playlistinfo', this.imprintPlaylistinfo );
			this.onImprint( 'status', this.imprintStatus );
			
			this.on( 'click', {
				'clearButtonSelector': this.clickClear,
				'consumeButtonSelector': this.clickConsume,
				'removeButtonSelector': this.clickRemove,
				'playNowButtonSelector': this.clickPlayNow,
				'pauseButtonSelector': this.clickPause
			} );

			this.render();
		} );

		this.clickClear = function() {
			this.trigger( document, 'request-clear' );
		};

		this.clickConsume = function() {
			if ( this.attr.imprints.status.consume == true ) {
				this.trigger( document, 'request-consume', { state: false } );
			}
			else {
				this.trigger( document, 'request-consume', { state: true } );
			}
		};

		this.clickRemove = function( e, d ) {
			this.trigger( document, 'request-delete', {
				pos: jquery( d.el ).data('pos')
			} );
		};

		this.clickPlayNow = function( e, d ) {
			this.trigger( document, 'request-play', {
				pos: jquery( d.el ).data('pos')
			} );
		};

		this.clickPause = function() {
			console.log('no pausing! LISTENING ONLY!');
		};

		this.imprintCurrentsong = function() {
			this.updatePlaylistinfo();
			this.render();
		};

		this.imprintPlaylistinfo = function() {
			this.updatePlaylistinfo();
			this.render();
		};

		this.imprintStatus = function() {
			this.updateConsumeButton();
		};

		this.updatePlaylistinfo = function() {
			var position = this.attr.imprints.currentsong.Pos;
			this.attr.imprints.playlistinfo.results =
				this.attr.imprints.playlistinfo.result.map( function( track ) {
				if ( track.Pos == position ) {
					track.playing = true;
				}
				else {
					track.playing = false;
				}

				return track;
			} );
		};

		this.updateConsumeButton = function() {
			var btn = this.select('consumeButtonSelector');
			if ( this.attr.imprints.status.consume ) {
				btn.addClass( 'btn-primary' );
			}
			else {
				btn.removeClass( 'btn-primary' );
			}
		};

	}

} );
