define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery'),
	    templates = require('js/templates'),
	    wsmpd = require('wsmpd');

	return defineComponent( queue );

	function queue() {

		this.after('initialize', function() {
			this.on( document, 'list-artist', this.onListArtist );

			this.renderList();
		} );

		this.title = "Artists";
		this.list = [];

		this.clickRow = function( e ) {
			console.log( 'click', jquery( e.delegateTarget ) );
		};

		this.onListArtist = function( e, artistList ) {
			this.updateArtistList( artistList.result );
			this.renderList();
		};

		this.updateArtistList = function( artistList ) {
			this.list = artistList.map( function( elem ) {
				return { title: elem.artist };
			} );
		};

		this.renderList = function() {
			this.$node.html( templates.list_panel.render( this ) );

			jquery('button').tooltip();
		};
	}

} );
