define( function( require ) {
	var jquery = require( 'jquery' );
	
	require( 'bootstrap' );

	require( 'js/nav' ).attachTo( 'nav ul.nav' );
	require( 'js/mini-control' ).attachTo( '#mini-control-container' );
	require( 'js/now-playing' ).attachTo( '#now-playing .jumbotron' );
	require( 'js/queue' ).attachTo( '#queue-container' );
	require( 'js/search' ).attachTo( '#search-container' );
	require( 'js/artists' ).attachTo( '#artists-container' );
	require( 'js/albums' ).attachTo( '#albums-container' );
	require( 'js/outputs' ).attachTo( '#settings-outputs-container' );

	require( 'js/data/search' ).attachTo( document );
	require( 'js/data/wsmpd' ).attachTo( document );

	jquery(document).on( 'currentsong', function(e, msg) {
		jquery(document).trigger('request-playlistinfo');
	} );

	return function() {
		jquery('button').tooltip();
	};
} );
