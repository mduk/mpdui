define( function( require ) {
	'use strict';
	
	var defineComponent = require('flight/component'),
	    jquery = require('jquery');

	return defineComponent( connectionStatus,
		require('mixin/template')
	);

	function connectionStatus() {

		this.defaultAttrs( {
			withTemplate: 'connection_status',
			
			statusBtnSelector: '#connectionstatus-status',
			statusIconSelector: 'span.glyphicon'
		} );

		this.after('initialize', function() {
			this.on( document, 'websocket-connect', this.onConnect );
			this.on( document, 'websocket-disconnect', this.onDisconnect );

			this.render();

			this.on( 'click', {
				statusBtnSelector: this.clickStatus
			} );

		});

		this.connectedState = false;

		this.onConnect = function() {
			this.updateStatusButton( true );
		};
		
		this.onDisconnect = function() {
			this.updateStatusButton( false );
		};

		this.clickStatus = function() {
			var host = "ws://" + window.location.host + "/websocket";
			switch ( this.connectedState ) {
				case false:
					this.trigger( document, 'request-connect', { host: host } );
					break;

				case true:
					//this.trigger( document, 'request-disconnect', { host: host } );
					break;
			}
		};

		this.updateStatusButton = function( state ) {
			if ( state == this.connectedState ) {
				return;
			}
			
			var btn = this.select('statusBtnSelector');
			var icon = this.select('statusIconSelector');
			
			switch ( state ) {
				case false:
					btn.addClass('btn-danger');
					btn.removeClass('btn-success');

					icon.removeClass('glyphicon-ok');
					icon.addClass('glyphicon-remove');
					break;

				case true:
					btn.removeClass('btn-danger');
					btn.addClass('btn-success');

					icon.removeClass('glyphicon-remove');
					icon.addClass('glyphicon-ok');
					break;
			}

			this.connectedState = state;
		};
	}

} );
