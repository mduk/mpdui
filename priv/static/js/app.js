define( function( require ) {
	var jquery = require( 'jquery' );
	
	require( 'bootstrap' );

	require( 'js/ui/nav' ).attachTo( 'nav ul.nav' );
	require( 'js/ui/mini-control' ).attachTo( '#mini-control-container' );
	require( 'js/ui/now-playing' ).attachTo( '#now-playing .jumbotron' );
	require( 'js/ui/queue' ).attachTo( '#queue-container' );
	require( 'js/ui/search' ).attachTo( '#search-container' );
	require( 'js/ui/artists' ).attachTo( '#artists-container' );
	require( 'js/ui/albums' ).attachTo( '#albums-container' );
	require( 'js/ui/outputs' ).attachTo( '#settings-outputs-container' );

	require( 'js/data/search' ).attachTo( document );
	require( 'js/data/wsmpd' ).attachTo( document );

	return function() {
		jquery('button').tooltip();
	};
} );
