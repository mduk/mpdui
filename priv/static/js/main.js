require.config( {
	baseUrl: '/static/',
	paths: {
		"jquery"   : "bower_components/jquery/dist/jquery.min",
		"bootstrap": "bower_components/bootstrap/dist/js/bootstrap.min",
		"hogan"    : "bower_components/hogan.js/web/builds/3.0.0/hogan-3.0.0.amd",
		"flight"   : "bower_components/flight/lib",
		"wsmpd"    : "js/wsmpd",
	},
	shim: {
		"bootstrap": {
			deps: [ "jquery" ]
		}
	},
	waitSeconds: 10
} );

require( [ 'js/app' ], function( app ) {
	app();
} );
