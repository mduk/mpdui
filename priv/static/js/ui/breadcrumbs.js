define( function( require ) {
	'use strict';

	var defineComponent = require('flight/component'),
	    jquery = require('jquery');

	return defineComponent( breadcrumbs );

	function breadcrumbs() {

		this.defaultAttrs( {
			listSelector: 'ol.breadcrumbs',
			listItemLinkSelector: 'li'
		} );

		this.after('initialize', function() {
			this.on( document, 'list-artist', this.onListArtist );
			this.on( document, 'list-album-artist', this.onListAlbumArtist );
			this.on( document, 'find-album', this.onFindAlbum );
			this.on( 'click', {
				'listItemLinkSelector': this.onClickItemLink
			} );
		} );

		this.artist = '';
		this.album = '';

		this.crumbs = {
			artists: {
				render: function() {
					if ( this.artist == '' && this.album == '' ) {
						return this.renderActiveItem( { text: 'Artists' } );
					}
					return this.renderLinkItem( { text: 'Artists' } );
				},
				click: function( e, d ) {
					this.trigger( document, 'request-list-artist' );
				}
			},
			albums: {
				render: function() {
					if ( this.artist != '' && this.album == '' ) {
						return this.renderActiveItem( { text: this.artist } );
					}
					if ( this.artist != '' && this.album != '' ) {
						return this.renderLinkItem( { text: this.artist } );
					}
					return '';
				},
				click: function( e, d ) {
					this.album = '';
					this.trigger( document, 'request-list-artist-albums', { artist: this.artist } );
				}
			},
			tracks: {
				render: function() {
					if ( this.album != '' ) {
						return this.renderActiveItem( { text: this.album } );
					}
					return '';
				},
				click: function( e, d ) {}
			}
		};

		this.onClickItemLink = function( e, d ) {
			e.preventDefault();
			var crumb = $( d.el ).attr('data-crumb');
			this.crumbs[ crumb ].click.call( this );
		};

		this.onListArtist = function( e, d ) {
			this.artist = '';
			this.album = '';
			this.render();
		};

		this.onListAlbumArtist = function( e, d ) {
			this.artist = d.command.args[2];
			this.render();
		};

		this.onFindAlbum = function( e, d ) {
			this.album = d.command.args[1];
			this.render();
		};

		this.render = function( breadcrumbs ) {
			this.$node.html( this.renderBreadcrumbs() );
		};

		this.renderBreadcrumbs = function() {
			var rendered = $('<ol class="breadcrumb"></ol>');
			for ( var i in this.crumbs ) {
				var crumb = $( this.crumbs[i].render.call( this ) );
				crumb.attr('data-crumb', i);
				rendered.append( crumb );
			}
			return rendered;
		};

		this.renderActiveItem = function( item ) {
			return '<li class="active">' + item.text + '</li>';
		};

		this.renderLinkItem = function( item ) {
			var fire = ( typeof item.fire != 'undefined' ) ? item.fire : '';
			return '<li><a href="" data-fire="' + fire + '">' + item.text + '</a></li>';
		};

	}

} );
