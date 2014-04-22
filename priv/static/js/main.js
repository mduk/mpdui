require.config( {
    paths: {
        "jquery": "/static/bower_components/jquery/dist/jquery.min",
    },
    waitSeconds: 10
} );

require( [ 'jquery' ], function( jquery ) {
    console.log("requirejs ready");
});
