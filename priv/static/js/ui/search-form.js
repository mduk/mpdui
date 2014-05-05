define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery');

	return defineComponent( search,
		require('mixin/template')
	);

	function search() {

		this.defaultAttrs( {
			withTemplate: 'search-form',

			searchFormSelector: 'form',
			searchInputSelector: 'form input[type=text]',
		} );

		this.after('initialize', function() {
			this.render();

			this.on( 'submit', { 'searchFormSelector': this.submitSearch } );
			this.on( 'keyup', { 'searchInputSelector': this.keyupSearchInput } );
		} );

		this.keyupSearchInput = function( e, d ) {
			var terms = jquery( d.el ).val();
			if ( terms.length >=3 && terms[terms.length - 1] == ' ' ) {
				this.submitSearch();
			}
		};

		this.submitSearch = function( e ) {
			e.preventDefault();

			var terms = this.select('searchInputSelector').val().trim();

			this.trigger( document, 'request-search', {
				type: 'title',
				what: terms
			} );

			this.trigger( document, 'request-search', {
				type: 'album',
				what: terms
			} );

			this.trigger( document, 'request-search', {
				type: 'artist',
				what: terms
			} );
		};
	}

} );
