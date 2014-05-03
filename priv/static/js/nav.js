define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery');

	return defineComponent( nav );

	function nav() {

		this.after('initialize', function() {
			this.on( 'ul.nav a[data-toggle=tab]', 'click', this.clickTab );
			this.on( document, 'change-tab', this.onChangeTab );
		} );

		this.onChangeTab = function( e, d ) {
			jquery('#nav-' + d.to).tab('show');
		};

		this.clickTab = function( e ) {
			var tab = jquery( e.currentTarget );

			this.trigger( 'tab-change', {
				'$from': this.active,
				'$to': tab
				
			} );

			this.active = tab;
		};
	}

} );
