define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery');

	return defineComponent( albums,
		require('mixin/template')
	);

	function albums() {

		this.defaultAttrs( {
			withTemplate: 'albums',

			addToQueueButtonSelector: 'button.add-to-queue',
			viewAlbumButtonSelector: 'button.view-album',

			albums: []
		} );

		this.after('initialize', function() {
			this.on( document, 'list-album', this.onListAlbum );
			this.on( document, 'tab-change', this.onTabChange );

			this.render();

			this.on( 'click', {
				'addToQueueButtonSelector': this.clickAddToQueue,
				'viewAlbumButtonSelector': this.clickViewAlbum
			} );
		} );

		this.clickViewAlbum = request( 'request-search' );

		this.clickAddToQueue = request( 'request-findadd' );

		this.onListAlbum = function( e, albumList ) {
			this.updateAlbumList( albumList.result );
			this.render();
		};

		this.onTabChange = function( e, msg ) {
			if ( msg.$to.attr('id') == 'nav-albums' ) {
				this.trigger( document, 'request-list', {
					type: 'album'
				} );
			}
		};

		this.updateAlbumList = function( albumList ) {
			this.attr.albums = albumList.map( function( elem ) {
				return { title: elem.album };
			} );
		};

		function request( event ) {
			return function( e, d ) {
				this.trigger( document, event, {
					type: 'album',
					what: jquery( d.el ).data('album')
				} );
			};
		}
	}

} );
