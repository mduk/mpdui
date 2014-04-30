define(function(require) {
	return function() {

		templates = require('js/templates');

		this.render = function() {
			if ( typeof this.preRender == 'function' ) {
				this.preRender();
			}

			this.$node.html(
				templates[this.attr.withTemplate].render(this)
			);

			if ( typeof this.postRender == 'function' ) {
				this.postRender();
			}
		};

	};
});
