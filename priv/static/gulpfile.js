var gulp = require('gulp');
var compiler = require('gulp-hogan-compile');

gulp.task( 'default', [ 'templates' ] );

gulp.task( 'templates', function() {
	gulp.src('templates/**/*.html')
	    .pipe( compiler('templates.js') )
	    .pipe( gulp.dest('js/') );
} );

gulp.task( 'watch', function() {
	gulp.watch( 'templates/**/*.html', [ 'templates' ] );
} );
