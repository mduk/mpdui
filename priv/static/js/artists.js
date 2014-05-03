define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery'),
	    nav = require('js/nav'),
	    wsmpd = require('wsmpd');

	return defineComponent( artists,
		require('js/withTemplate')
	);

	function artists() {

		this.defaultAttrs( {
			withTemplate: 'artists',

			addToQueueButtonSelector: '#artists-container button.add-to-queue',
			viewAlbumButtonSelector: '#artists-container button.view-album'
		} );

		this.after('initialize', function() {
			this.on( document, 'list-artist', this.onListArtist );
			this.on( nav, 'tab-change', this.onTabChange );

			this.render();

			this.on( 'click', {
				'addToQueueButtonSelector': this.clickAddToQueue,
				'viewAlbumButtonSelector': this.clickViewAlbum
			} );
		} );

		this.title = "Artists";
		this.artists = [];

		this.clickViewAlbum = function( e, d ) {
			this.trigger( document, 'request-search', {
				type: 'artist',
				what: jquery( d.el ).data('artist')
			} );
		};

		this.clickAddToQueue = function( e, d ) {
			var artist = jquery( d.el ).data('artist');
			wsmpd.findadd( 'artist', artist );	
		};

		this.onListArtist = function( e, artistList ) {
			this.updateArtistList( artistList.result );
			this.render();
		};

		this.onTabChange = function( e, msg ) {
			if ( msg.$to.attr('id') == 'nav-artists' ) {
				wsmpd.list('artist');
			}
		};

		this.updateArtistList = function( artistList ) {
			this.artists = artistList.map( function( elem ) {
				return { title: elem.artist };
			} );
		};
	}

} );
