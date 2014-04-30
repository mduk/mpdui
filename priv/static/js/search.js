define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery'),
	    wsmpd = require('wsmpd');

	return defineComponent( search,
		require('js/withTemplate')
	);

	function search() {

		this.defaultAttrs( {
			withTemplate: 'search'
		} );

		this.after('initialize', function() {
			this.on( document, 'search', this.onSearch );

			this.render();
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

			this.render();
		};

		this.postRender = function() {
			this.on( '#search-container form', 'submit', this.submitSearch );
			this.on( '#search-container table button', 'click', this.clickAddToQueue );
		};
	}

} );
