define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery'),
	    templates = require('js/templates'),
	    wsmpd = require('wsmpd');

	return defineComponent( queue );

	function queue() {

		this.after('initialize', function() {
			this.on( document, 'album-list', this.onAlbumList );

			this.renderList();
		} );

		this.title = "Albums";
		this.list = [];

		this.clickRow = function( e ) {
			console.log( 'click', jquery( e.delegateTarget ) );
		};

		this.onAlbumList = function( e, albumList ) {
			this.updateAlbumList( albumList.result );
			this.renderList();
		};

		this.updateAlbumList = function( albumList ) {
			this.list = albumList.map( function( elem ) {
				return { title: elem.album };
			} );
		};

		this.renderList = function() {
			this.$node.html( templates.list_panel.render( this ) );

			jquery('button').tooltip();
		};
	}

} );
