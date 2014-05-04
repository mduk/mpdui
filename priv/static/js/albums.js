define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery'),
	    nav = require('js/nav'),
	    wsmpd = require('wsmpd');

	return defineComponent( albums,
		require('mixin/template')
	);

	function albums() {

		this.defaultAttrs( {
			withTemplate: 'albums',

			addToQueueButtonSelector: '#albums-container button.add-to-queue',
			viewAlbumButtonSelector: '#albums-container button.view-album'
		} );

		this.after('initialize', function() {
			this.on( document, 'list-album', this.onListAlbum );
			this.on( nav, 'tab-change', this.onTabChange );

			this.render();

			this.on( 'click', {
				'addToQueueButtonSelector': this.clickAddToQueue,
				'viewAlbumButtonSelector': this.clickViewAlbum
			} );
		} );

		this.title = "Albums";
		this.albums = [];

		this.clickViewAlbum = function( e, d ) {
			this.trigger( document, 'request-search', {
				type: 'album',
				what: jquery( d.el ).data('album')
			} );
		};

		this.clickAddToQueue = function( e, d ) {
			var album = jquery( d.el ).data('album');
			wsmpd.findadd( 'album', album );	
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
	}

} );
