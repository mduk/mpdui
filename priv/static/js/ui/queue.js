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
			removeButtonSelector: 'button.remove-from-queue',
			playNowButtonSelector: 'button.play-now',
			pauseButtonSelector: 'button.pause'
		} );

		this.after('initialize', function() {
			this.on( document, 'status', this.onStatus );
			this.on( document, 'currentsong', this.onCurrentsong );
			this.on( document, 'playlistinfo', this.onPlaylistinfo );
			
			this.on( 'click', {
				'clearButtonSelector': this.clickClear,
				'removeButtonSelector': this.clickRemove,
				'playNowButtonSelector': this.clickPlayNow,
				'pauseButtonSelector': this.clickPause
			} );

			this.render();
		} );

		this.state = '';
		this.position = 0;
		this.playlistinfo = [];

		this.clickClear = function() {
			this.trigger(document, 'request-clear');
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

		this.onStatus = function( e, status ) {
			if ( this.state == status.state ) {
				return;
			}
			
			this.state = status.state;
			this.render();
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

		this.postRender = function() {
			this.on( '#queue-clear', 'click', this.clickClear );
			this.on( '#queue-container a', 'click', this.clickTrack );
		};
	}

} );
