define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery'),
	    nav = require('js/nav'),
	    wsmpd = require('wsmpd');

	return defineComponent( albums,
		require('js/withTemplate')
	);

	function albums() {

		this.defaultAttrs( {
			withTemplate: 'albums'
		} );

		this.after('initialize', function() {
			this.on( document, 'list-album', this.onListAlbum );
			this.on( nav, 'tab-change', this.onTabChange );

			this.render();
		} );

		this.title = "Albums";
		this.albums = [];

		this.clickAddToQueue = function( e ) {
			var album = jquery(e.currentTarget).data('album');
			wsmpd.findadd( 'album', album);	
		};

		this.onListAlbum = function( e, albumList ) {
			this.updateAlbumList( albumList.result );
			this.render();
		};

		this.onTabChange = function( e, msg ) {
			if ( msg.$to.attr('id') == 'nav-albums' ) {
				wsmpd.list('album');
			}
		};

		this.updateAlbumList = function( albumList ) {
			this.albums = albumList.map( function( elem ) {
				return { title: elem.album };
			} );
		};

		this.postRender = function() {
			this.on( '#albums-container button.add-to-queue', 'click', this.clickAddToQueue );
		};
	}

} );
