define( ['flight/component'], function( defineComponent ) {
	'use strict';

	console.log('defining component');

	return defineComponent( function() {
		this.fooBar = function() {
			alert('foo!');
		};

		this.after('initialize', function() {
			this.on( document, 'foobar', this.fooBar );
		});
	} );
	
	
} );
