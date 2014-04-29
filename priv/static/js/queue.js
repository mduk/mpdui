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

		this.onStatus = function( e, status ) {
			if ( this.state == status.state ) {
				return;
			}
			
			this.state = status.state;
			this.renderQueue();
		};

		this.onCurrentsong = function( e, currentsong ) {
			this.position = currentsong.Pos;
			this.renderQueue();
		};

		this.onPlaylistinfo = function( e, playlistinfo ) {
			this.playlistinfo = playlistinfo.playlistinfo; // 'cause flightjs gets funky with non-object values apparently
			this.renderQueue();
		};

		this.renderQueue = function() {
			if ( typeof this.playlistinfo[this.position] == 'object' ) {
				this.playlistinfo[this.position].playing = true;
			}
			this.$node.html( templates.queue.render( this ) );
		};
	}

} );
