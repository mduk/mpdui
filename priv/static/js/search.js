define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery'),
	    templates = require('js/templates'),
	    wsmpd = require('wsmpd');

	return defineComponent( queue );

	function queue() {

		this.after('initialize', function() {
			this.on( document, 'search', this.onSearch );

			this.renderLibrary();
		} );

		this.searchType = 'artist';
		this.searchTerm = '';
		this.results = [];

		this.submitSearch = function() {
			wsmpd.search(
				this.$node.find("input[type=hidden]").val(),
				this.$node.find("input[type=text]").val()
			);
		};

		this.clickAddToQueue = function( e ) {
			wsmpd.addid( jquery( e.delegateTarget ).data('songid') );
			wsmpd.playlistinfo();
		};

		this.onSearch = function( e, msg ) {
			this.searchType = msg.command.args[0];
			this.searchTerm = msg.command.args[1];
			this.results = msg.result;

			this.renderLibrary();
		};

		this.renderLibrary = function() {
			this.$node.html( templates.search.render( this ) );
			
			this.on( '#search-container form', 'submit', this.submitSearch );
			this.on( '#search-container table button', 'click', this.clickAddToQueue );
		};
	}

} );
