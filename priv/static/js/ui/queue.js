define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery');

	return defineComponent(
		require('mixin/template'),
		queue
	);

	function queue() {

		this.defaultAttrs( {
			withTemplate: 'queue',

			clearButtonSelector: 'button.clear-queue',
			consumeButtonSelector: 'button.consume-queue',
			removeButtonSelector: 'button.remove-from-queue',
			playNowButtonSelector: 'button.play-now',
			pauseButtonSelector: 'button.pause',

			state: '',
			consume: false,
			position: 0,
			playlistinfo: []
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

		this.clickClear = function() {
			this.trigger(document, 'request-clear');
		};

		this.clickConsume = function() {
			if ( this.attr.consume == true ) {
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
			this.attr.position = currentsong.Pos;
			this.updatePlaylistinfo();
			this.render();
		};

		this.onPlaylistinfo = function( e, playlistinfo ) {
			this.attr.playlistinfo = playlistinfo.result;
			this.updatePlaylistinfo();
			this.render();
		};

		this.onStatus = function( e, status ) {
			if ( typeof status.consume != 'undefined' ) {
				this.attr.consume = status.consume;
				this.updateConsumeButton();
			}
		};

		this.updatePlaylistinfo = function() {
			var position = this.attr.position;
			this.attr.playlistinfo = this.attr.playlistinfo.map( function( track ) {
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
			if ( this.attr.consume == true ) {
				btn.addClass( 'btn-primary' );
			}
			else {
				btn.removeClass( 'btn-primary' );
			}
		};

	}

} );
