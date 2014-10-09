/**
 * Event Imprint
 *
 * Allows components to "subscribe" to event payload properties such that
 * the component's local copies of the properties is updated whenever the
 * event is received.
 *
 * Imprint handlers can be registered which will be called after imprinting
 * the event onto the component.
 */
define(function(require) {
	return function() {

		/**
		 * Object of imprint handlers by event name.
		 * { eventName : array<handler>, ... }
		 */
		this.imprintHandlers = {};

		/**
		 * Build and register the imprint handlers to the specified events
		 */
		this.after( 'initialize', function() {
			var imprintEvents = this.attr.imprintEvents;
			var imprints = this.attr.imprints;

			for ( var eventName in imprintEvents ) {
				var eventProperties = imprintEvents[ eventName ];
				var eventHandler = imprinterFactory( eventName, eventProperties );
				this.on( document, eventName, eventHandler );
			}
		} );

		/**
		 * Add a single imprint handler
		 */
		this.addImprintHandler = function( eventName, handler ) {
			if ( typeof this.imprintHandlers[ eventName ] == 'undefined' ) {
				this.imprintHandlers[ eventName ] = [];
			}
			this.imprintHandlers[ eventName ].push( handler );
		}	

		/**
		 * Register imprint handler
		 *
		 * Imprint handlers are called after an event has been imprinted onto a component.
		 * They can be assigned one at a time, eventName can be an array of event names, or
		 * could be an object of event names to handlers.
		 */
		this.onImprint = function( eventName, handler ) {

			// assigning a single handler to an event
			if ( typeof eventName == 'string' && typeof handler == 'function' ) {
				this.addImprintHandler( eventName, handler );
			}

			// assigning a single handler to many events
			if ( typeof eventName == 'array' && typeof handler == 'function' ) {
				for ( var i in eventName ) {
					var name = eventName[ i ];
					this.addImprintHandler( name, handler );
				}
			}

			// assigning a map of handlers
			if ( typeof eventName == 'object' && typeof handler == 'undefined' ) {
				for ( var name in eventName ) {
					if ( typeof eventName[ name ] == 'object' ) {
						for ( var handler in eventName[ name ] ) {
							this.addImprintHandler( name, eventName[ name ][ handler ] );
						}
					}

					if ( typeof eventName[ name ] == 'function' ) {
						this.addImprintHandler( name, eventName[ name ] );
					}
				}
			}
		}

		/**
		 * Build imprinter event handlers. The imprinter is what will copy
		 * all available event properties to the component attributes and
		 * call any imprint handlers that may have been registered.
		 */
		function imprinterFactory( eventName, eventProperties ) {
			return function( event, data ) {

				var imprintHandlers = [];
				if ( typeof this.imprintHandlers != 'undefined' ) {
					imprintHandlers = this.imprintHandlers[ eventName ];
				}

				for ( var i in eventProperties ) {
					var property = eventProperties[ i ];
					if ( typeof data[ property ] != 'undefined' ) {
						this.attr.imprints[ eventName ][ property ] = data[ property ];
					}
				}

				for ( var i in imprintHandlers ) {
					imprintHandlers[ i ].call( this, [ this.attr.imprints[ eventName ] ] );
				}
			};
		};

	};
});
