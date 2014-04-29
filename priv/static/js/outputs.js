define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery'),
	    templates = require('js/templates'),
	    wsmpd = require('wsmpd');

	return defineComponent( queue );

	function queue() {

		this.after('initialize', function() {
			this.renderOutputs();

			this.on( document, 'outputs', this.onOutputs );
		} );

		this.title = "Outputs";
		this.list = [];

		this.onOutputs = function( e, message ) {
			this.updateList( message.result );
			this.renderOutputs();
		};

		this.updateList = function( outputs ) {
			this.list = outputs.map( function( elem ) {
				var label = 'Enabled';
				if ( elem.outputenabled == 0 ) {
					label = 'disabled';
				}
				
				return {
					title: elem.outputname,
					body: label
				};
			} );
		};

		this.renderOutputs = function() {
			this.$node.html( templates.list_panel.render( this ) );
		};
	}

} );
