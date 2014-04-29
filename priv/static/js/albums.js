define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery'),
	    templates = require('js/templates'),
	    nav = require('js/nav'),
	    wsmpd = require('wsmpd');

	return defineComponent( queue );

	function queue() {

		this.after('initialize', function() {
			this.on( document, 'list-album', this.onListAlbum );
			this.on( nav, 'tab-change', this.onTabChange );

			this.renderList();
		} );

		this.title = "Albums";
		this.albums = [];

		this.clickAddToQueue = function( e ) {
			var album = jquery(e.currentTarget).data('album');
			wsmpd.findadd( 'album', album);	
		};

		this.onListAlbum = function( e, albumList ) {
			this.updateAlbumList( albumList.result );
			this.renderList();
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

		this.renderList = function() {
			this.$node.html( templates.albums.render( this ) );

			jquery('button').tooltip();

			this.on( '#albums-container button.add-to-queue', 'click', this.clickAddToQueue );
		};
	}

} );
