define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery'),
	    wsmpd = require('wsmpd');

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
			this.on( document, 'findadd clear', this.refreshQueue );
			
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

		this.clickTrack = function( e ) {
			wsmpd.play( jquery( e.delegateTarget ).data('pos') );
		};

		this.clickClear = function() {
			wsmpd.clear();
			wsmpd.playlistinfo();
		};

		this.clickRemove = function( e, d ) {
			wsmpd.delete( jquery( d.el ).data('pos') );
			wsmpd.playlistinfo();
		};

		this.clickPlayNow = function( e, d ) {
			wsmpd.play( jquery( d.el ).data('pos') );
			wsmpd.playlistinfo();
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

		this.refreshQueue = function() {
			wsmpd.playlistinfo();
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
