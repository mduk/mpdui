define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery'),
	    wsmpd = require('wsmpd'),
	    nav = require('js/nav');

	return defineComponent( outputs,
		require('mixin/template')
	);

	function outputs() {

		this.defaultAttrs( {
			withTemplate: 'list_panel'
		} );

		this.after('initialize', function() {
			this.render();

			this.on( document, 'outputs', this.onOutputs );
			this.on( nav, 'tab-change', this.onTabChange );
		} );

		this.title = "Outputs";
		this.list = [];

		this.onOutputs = function( e, message ) {
			this.updateList( message.result );
			this.render();
		};

		this.onTabChange = function( e, msg ) {
			if ( msg.$to.attr('id') == 'nav-settings' ) {
				this.trigger( document, 'request-outputs' );
			}
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
	}

} );
