define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery');

	return defineComponent( queue,
		require('mixin/template')
	);

	function queue() {

		this.defaultAttrs( {
			withTemplate: 'queue',

			clearButtonSelector: 'button.clear-queue',
			consumeButtonSelector: 'button.consume-queue',
			removeButtonSelector: 'button.remove-from-queue',
			playNowButtonSelector: 'button.play-now',
			pauseButtonSelector: 'button.pause'
		} );

		this.after('initialize', function() {
			this.on( document, 'currentsong', this.onCurrentsong );
			this.on( document, 'playlistinfo', this.onPlaylistinfo );
			this.on( document, 'status', this.onStatus );
			
			this.on( 'click', {
				'clearButtonSelector': this.clickClear,
				'consumeButtonSelector': this.clickConsume,
				'removeButtonSelector': this.clickRemove,
				'playNowButtonSelector': this.clickPlayNow,
				'pauseButtonSelector': this.clickPause
			} );

			this.render();
		} );

		this.state = '';
		this.consume = false;
		this.position = 0;
		this.playlistinfo = [];

		this.clickClear = function() {
			this.trigger(document, 'request-clear');
		};

		this.clickConsume = function() {
			if ( this.consume == true ) {
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

		this.onCurrentsong = function( e, currentsong ) {
			this.position = currentsong.Pos;
			this.updatePlaylistinfo();
			this.render();
		};

		this.onPlaylistinfo = function( e, playlistinfo ) {
			this.playlistinfo = playlistinfo.result;
			this.updatePlaylistinfo();
			this.render();
		};

		this.onStatus = function( e, status ) {
			if ( typeof status.consume != 'undefined' ) {
				this.consume = status.consume;
				this.updateConsumeButton();
			}
		};

		this.updatePlaylistinfo = function() {
			var position = this.position;
			this.playlistinfo = this.playlistinfo.map( function( track ) {
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
			if ( this.consume == true ) {
				btn.addClass( 'btn-primary' );
			}
			else {
				btn.removeClass( 'btn-primary' );
			}
		};

		this.postRender = function() {
			this.on( '#queue-clear', 'click', this.clickClear );
			this.on( '#queue-container a', 'click', this.clickTrack );
		};
	}

} );
