define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery'),
		templates = require('js/templates');

	return defineComponent( browse );

	function browse() {

		this.defaultAttrs( {
			addToQueueButtonSelector: 'button.add-to-queue',
			artistRowSelector: 'tr.artist',
			albumRowSelector: 'tr.album'
		} );

		this.after('initialize', function() {
			this.on( document, 'tab-change', this.onTabChange );
			this.on( document, 'list-artist', this.onListArtist );
			this.on( document, 'list-album-artist', this.onListAlbumArtist );
			this.on( document, 'find-album', this.onFindAlbum );

			this.on( 'click', {
				'addToQueueButtonSelector': this.onClickAddToQueueButton,
				'artistRowSelector': this.onClickArtistRow,
				'albumRowSelector': this.onClickAlbumRow
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
			this.$node.html( templates['artists'].render( {
				artists: d.result.map( function( elem ) {
					return { title: elem.artist }
				} )
			}, templates ) );
		};

		// View Artist button was clicked
		//	Request artist albums list
		this.onClickArtistRow = function( e, d ) {
			var artist = jquery( d.el ).data('artist');
			this.trigger( document, 'request-list-artist-albums', {
				artist: artist
			} );
		};

		// Received artist albums list
		//	Render the album list
		this.onListAlbumArtist = function( e, d ) {
			this.$node.html( templates['albums'].render( {
				albums: d.result.map( function( elem ) {
					return { title: elem.album }
				} )
			}, templates ) );
		};

		// Clicked view album button
		//	Request the track list
		this.onClickAlbumRow = function( e, d ) {
			var album = jquery( d.el ).data('album');
			this.trigger( document, 'request-find', {
				type: 'album',
				what: album
			} );
		};

		// Recieved the track list
		//	Render the track list
		this.onFindAlbum = function( e, d ) {
			this.$node.html( templates['tracks'].render( {
				tracks: d.result.map( function( elem ) {
					return { title: elem.Title, file: elem.file }
				} )
			}, templates ) );
		};

		// Click the add button
		//	Request playlist append
		this.onClickAddToQueueButton = function( e, d ) {
			this.trigger( document, 'request-addid', {
				id: jquery( d.el ).data('songid')
			} );
		}
	}

} );
