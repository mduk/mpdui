define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery'),
	    templates = require('js/templates'),
	    wsmpd = require('wsmpd');

	return defineComponent( queue );

	function queue() {

		this.after('initialize', function() {
			this.on( document, 'status', this.onStatus );
			this.on( document, 'currentsong', this.onCurrentsong );
			this.on( document, 'playlistinfo', this.onPlaylistinfo );

			this.renderQueue();
		} );

		this.state = '';
		this.position = 0;
		this.playlistinfo = [];

		this.clickTrack = function( e ) {
			wsmpd.play( jquery( e.delegateTarget ).data('pos') );
		};

		this.clickClear = function() {
			wsmpd.clear();
		};

		this.onStatus = function( e, status ) {
			if ( this.state == status.state ) {
				return;
			}
			
			this.state = status.state;
			this.renderQueue();
		};

		this.onCurrentsong = function( e, currentsong ) {
			this.position = currentsong.Pos;
			this.updatePlaylistinfo();
			this.renderQueue();
		};

		this.onPlaylistinfo = function( e, playlistinfo ) {
			this.playlistinfo = playlistinfo.playlistinfo; // 'cause flightjs gets funky with non-object values apparently
			this.updatePlaylistinfo();
			this.renderQueue();
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

		this.renderQueue = function() {
			this.$node.html( templates.queue.render( this ) );

			this.on( '#queue-clear', 'click', this.clickClear );
			this.on( '#queue-container a', 'click', this.clickTrack );

			jquery('button').tooltip();
		};
	}

} );
