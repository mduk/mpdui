define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery'),
		templates = require('js/templates');

	return defineComponent( browse );

	function browse() {

		this.defaultAttrs( {
			viewArtistButtonSelector: 'button.view-artist',
			viewAlbumButtonSelector: 'button.view-album',
			addToQueueButtonSelector: 'button.add-to-queue'
		} );

		this.after('initialize', function() {
			this.on( document, 'tab-change', this.onTabChange );
			this.on( document, 'list-artist', this.onListArtist );
			this.on( document, 'list-album-artist', this.onListAlbumArtist );
			this.on( document, 'find-album', this.onFindAlbum );

			this.on( 'click', {
				'viewArtistButtonSelector': this.onClickViewArtistButton,
				'viewAlbumButtonSelector': this.onClickViewAlbumButton,
				'addToQueueButtonSelector': this.onClickAddToQueueButton
			} );
		} );

		// Tabbed to browse
		//	Request artist list
		this.onTabChange = function( e, d ) {
			console.log( 'tab-change', d.$to.attr('id') );
			if ( d.$to.attr('id') == 'nav-browse' ) {
				this.trigger( document, 'request-list', {
					type: 'artist'
				} );
			}
		};

		// Received an artist list
		//	Render the artist list
		this.onListArtist = function( e, d ) {
			console.log( 'list-artist', d.result );
			this.$node.html( templates['artists'].render( {
				artists: d.result.map( function( elem ) {
					return { title: elem.artist }
				} )
			}, templates ) );
		};

		// View Artist button was clicked
		//	Request artist albums list
		this.onClickViewArtistButton = function( e, d ) {
			var artist = jquery( d.el ).data('artist');

			console.log( 'click-view-artist', artist );

			this.trigger( document, 'request-list-artist-albums', {
				artist: artist
			} );
		};

		// Received artist albums list
		//	Render the album list
		this.onListAlbumArtist = function( e, d ) {
			console.log( 'list-album-artist', d );
			this.$node.html( templates['albums'].render( {
				albums: d.result.map( function( elem ) {
					return { title: elem.album }
				} )
			}, templates ) );
		};

		// Clicked view album button
		//	Request the track list
		this.onClickViewAlbumButton = function( e, d ) {
			console.log( 'click-view-album', d );
			this.trigger( document, 'request-find', {
				type: 'album',
				what: jquery( d.el ).data('album')
			} );
		};

		// Recieved the track list
		//	Render the track list
		this.onFindAlbum = function( e, d ) {
			console.log( 'find-album', d );
			this.$node.html( templates['tracks'].render( {
				tracks: d.result.map( function( elem ) {
					return { title: elem.Title, file: elem.file }
				} )
			}, templates ) );
		};

		// Click the add button
		//	Request playlist append
		this.onClickAddToQueueButton = function( e, d ) {
			console.log( 'click-add-to-queue', d );
			this.trigger( document, 'request-addid', {
				id: jquery( d.el ).data('songid')
			} );
		}
	}

} );
