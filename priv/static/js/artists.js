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
			withTemplate: 'list_panel'
		} );

		this.after('initialize', function() {
			this.on( document, 'list-artist', this.onListArtist );
			this.on( nav, 'view-change', this.onTabChange );

			this.render();
		} );

		this.title = "Artists";
		this.list = [];

		this.clickRow = function( e ) {
			console.log( 'click', jquery( e.delegateTarget ) );
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
			this.list = artistList.map( function( elem ) {
				return { title: elem.artist };
			} );
		};
	}

} );
