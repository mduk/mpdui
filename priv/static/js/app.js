define( function( require ) {
	var jquery = require( 'jquery' );

	return function() {

		require( 'bootstrap' );

		require( 'js/ui/nav' ).attachTo( 'nav ul.nav' );
		require( 'js/ui/mini-control' ).attachTo( '#mini-control-container' );
		require( 'js/ui/connection-status' ).attachTo( '#connection-status-container' );
		require( 'js/ui/now-playing' ).attachTo( '#now-playing-container' );
		require( 'js/ui/queue' ).attachTo( '#queue-container' );
		require( 'js/ui/search-form' ).attachTo( '#search-form-container' );
		require( 'js/ui/search-results' ).attachTo( '#search-results-container' );

		require( 'js/ui/browse' ).attachTo( '#browse-container' );
		require( 'js/ui/breadcrumbs' ).attachTo( '#breadcrumbs-container' );

		require( 'js/data/mpd' ).attachTo( document );

		require( 'js/keyboard-commands' ).attachTo( document );

		jquery('button').tooltip();

		jquery( document ).trigger( 'request-connect', {
			host: "ws://" + window.location.host + "/websocket"
		} );
	};
} );
