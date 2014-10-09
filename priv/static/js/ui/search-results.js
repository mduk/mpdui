define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery');

	return defineComponent( search,
		require('mixin/template')
	);

	function search() {

		this.defaultAttrs( {
			withTemplate: 'search-results',

			addToQueueBtnSelector: 'table button',

			searchTerm: '',
			results: []
		} );

		this.after('initialize', function() {
			this.on( document, 'search', this.onSearch );

			this.on( 'click', { 'addToQueueBtnSelector': this.clickAddToQueue } );
		} );

		this.clickAddToQueue = function( e, d ) {
			this.trigger( document, 'request-addid', {
				id: jquery( d.el ).data('songid')
			} );
		};

		this.onSearch = function( e, msg ) {
			var searchTerm = msg.command.args[1];

			if ( this.attr.searchTerm != searchTerm ) {
				this.attr.searchTerm = searchTerm;
				this.attr.results = [];
			}

			var storedResults = this.attr.results;
			var toMerge = msg.result.filter( function( track ) {
				for ( var i = 0; i < storedResults.length; i++ ) {
					if ( track.file = storedResults[i].file ) {
						return false;
					}
				}
				return true;
			} );

			jquery.merge( this.attr.results, toMerge );
			this.render();
		};
	}

} );
