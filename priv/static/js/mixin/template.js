define(function(require) {
	return function() {

		templates = require('js/templates');

		this.render = function() {
			this.$node.html(
				templates[this.attr.withTemplate].render(this, templates)
			);
		};

	};
});
