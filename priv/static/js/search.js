define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery'),
	    wsmpd = require('wsmpd');

	return defineComponent( search,
		require('mixin/template')
	);

	function search() {

		this.defaultAttrs( {
			withTemplate: 'search',

			searchFormSelector: '#search-container form',
			addToQueueBtnSelector: '#search-container table button'
		} );

		this.after('initialize', function() {
			this.on( document, 'search', this.onSearch );

			this.render();

			this.on( 'submit', { 'searchFormSelector': this.submitSearch } );
			this.on( 'click', { 'addToQueueBtnSelector': this.clickAddToQueue } );
		} );

		this.searchType = 'artist';
		this.searchTerm = '';
		this.results = [];

		this.submitSearch = function() {
			var terms = this.$node.find("input[type=text]").val();

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

		this.clickAddToQueue = function( e, d ) {
			wsmpd.addid( jquery( d.el ).data('songid') );
			wsmpd.playlistinfo();
		};

		this.onSearch = function( e, msg ) {
			var searchTerm = msg.command.args[1];

			if ( this.searchTerm != searchTerm ) {
				this.searchTerm = searchTerm;
				this.results = [];
			}

			jquery.merge( this.results, msg.result );
			this.render();
		};
	}

} );
