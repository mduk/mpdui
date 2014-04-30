define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery'),
	    wsmpd = require('wsmpd');

	return defineComponent( queue,
		require('js/withTemplate')
	);

	function queue() {

		this.defaultAttrs( {
			withTemplate: 'queue'
		} );

		this.after('initialize', function() {
			this.on( document, 'status', this.onStatus );
			this.on( document, 'currentsong', this.onCurrentsong );
			this.on( document, 'playlistinfo', this.onPlaylistinfo );
			this.on( document, 'findadd clear', this.refreshQueue );

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
