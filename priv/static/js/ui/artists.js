define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery');

	return defineComponent( artists,
		require('mixin/template')
	);

	function artists() {

		this.defaultAttrs( {
			withTemplate: 'artists',

			addToQueueButtonSelector: '#artists-container button.add-to-queue',
			viewAlbumButtonSelector: '#artists-container button.view-album'
		} );

		this.after('initialize', function() {
			this.on( document, 'list-artist', this.onListArtist );
			this.on( document, 'tab-change', this.onTabChange );

			this.render();

			this.on( 'click', {
				'addToQueueButtonSelector': this.clickAddToQueue,
				'viewAlbumButtonSelector': this.clickViewAlbum
			} );
		} );

		this.title = "Artists";
		this.artists = [];

		this.clickViewAlbum = request( 'request-search' )

		this.clickAddToQueue = request( 'request-findadd' );

		this.onListArtist = function( e, artistList ) {
			this.updateArtistList( artistList.result );
			this.render();
		};

		this.onTabChange = function( e, msg ) {
			if ( msg.$to.attr('id') == 'nav-artists' ) {
				this.trigger( document, 'request-list', {
					type: 'artist'
				} );
			}
		};

		this.updateArtistList = function( artistList ) {
			this.artists = artistList.map( function( elem ) {
				return { title: elem.artist };
			} );
		};

		function request( event ) {
			return function( e, d ) {
				this.trigger( document, event, {
					type: 'artist',
					what: jquery( d.el ).data('artist')
				} );
			};
		}
	}

} );
