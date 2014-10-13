define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery'),
		templates = require('js/templates');

	return defineComponent( browse );

	function browse() {

		this.defaultAttrs( {
			artistRowSelector: 'tr.artist',
			albumRowSelector: 'tr.album',
			trackRowSelector: 'tr.track'
		} );

		this.after('initialize', function() {
			this.on( document, 'tab-change', this.onTabChange );
			this.on( document, 'list-artist', this.onListArtist );
			this.on( document, 'list-album-artist', this.onListAlbumArtist );
			this.on( document, 'find-album', this.onFindAlbum );

			this.on( 'click', {
				'artistRowSelector': this.onClickArtistRow,
				'albumRowSelector': this.onClickAlbumRow,
				'trackRowSelector': this.onClickTrackRow
			} );
		} );

		// Tabbed to browse
		//	Request artist list
		this.onTabChange = function( e, d ) {
			if ( d.$to.attr('id') == 'nav-browse' ) {
				this.trigger( document, 'request-list', {
					type: 'artist'
				} );
			}
		};

		// Received an artist list
		//	Render the artist list
		this.onListArtist = function( e, d ) {
			this.$node.html( templates['listpanel'].render( {
				title: 'Artists',
				items: d.result.map( function( elem ) {
					return {
						class: 'artist',
						title: elem.artist,
						id: elem.artist
					}
				} )
			}, templates ) );
		};

		// View Artist button was clicked
		//	Request artist albums list
		this.onClickArtistRow = function( e, d ) {
			var artist = jquery( d.el ).data('id');
			this.trigger( document, 'request-list-artist-albums', {
				artist: artist
			} );
		};

		// Received artist albums list
		//	Render the album list
		this.onListAlbumArtist = function( e, d ) {
			this.$node.html( templates['listpanel'].render( {
				title: 'Albums',
				items: d.result.map( function( elem ) {
					return {
						class: 'album',
						title: elem.album,
						id: elem.album
					}
				} )
			}, templates ) );
		};

		// Clicked view album button
		//	Request the track list
		this.onClickAlbumRow = function( e, d ) {
			var album = jquery( d.el ).data('id');
			this.trigger( document, 'request-find', {
				type: 'album',
				what: album
			} );
		};

		// Recieved the track list
		//	Render the track list
		this.onFindAlbum = function( e, d ) {
			this.$node.html( templates['listpanel'].render( {
				title: 'Tracks',
				items: d.result.map( function( elem ) {
					return {
						class: 'track',
						title: elem.Title,
						id: elem.file
					}
				} )
			}, templates ) );
		};

		// Click the add button
		//	Request playlist append
		this.onClickTrackRow = function( e, d ) {
			this.trigger( document, 'request-addid', {
				id: jquery( d.el ).data('id')
			} );
		}
	}

} );
