define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery'),
	    templates = require('js/templates'),
	    wsmpd = require('wsmpd');

	return defineComponent( queue );

	function queue() {

		this.after('initialize', function() {
			this.on( document, 'search-results', this.onSearchResults );

			this.renderLibrary();
		} );

		this.searchType = 'artist';
		this.searchTerm = '';
		this.results = [];

		this.clickSearch = function() {
			wsmpd.search(
				this.$node.find("select").val(),
				this.$node.find("input[type=text]").val()
			);
		};

		this.onSearchResults = function( e, results ) {
			this.searchType = results.search[0];
			this.searchTerm = results.search[1];
			this.results = results.results;

			this.renderLibrary();
		};

		this.renderLibrary = function() {
			this.$node.html( templates.library.render( this ) );
			
			this.on( '#library-search', 'click', this.clickSearch );
		};
	}

} );
