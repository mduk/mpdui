define( function( require ) {
	var defineComponent = require( 'flight/component' ),
	    wsmpd = require( 'wsmpd' );
	
	return defineComponent( dataSearch );
	
	function dataSearch() {

		this.after('initialize', function() {

			this.on( document, 'request-search', function( e, d ) {
				wsmpd.search( d.type, d.what );
				this.trigger( document, 'change-tab', { to: 'search' } );
			} );

		} );

	}
} );