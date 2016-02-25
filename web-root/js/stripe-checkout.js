var stripe_handle = StripeCheckout.configure({
	key: 'pk_test_RuyrACKp8FG3rI4f26vgMu02',
	image: '/img/documentation/checkout/marketplace.png',
	locale: 'auto',
	token: function( token ) {
		// See token.id
	}
});

$( 'button.checkout' ).on( 'click', stripe_checkout );

function stripe_checkout( click_event ) {
	click_event.preventDefault();
	stripe_handle.open({
		name: 'Hex Cuff',
		description: 'REPLACE ME WITH REAL HEX CUFFS',
		amount: 2000
	});
}

$( window ).on( 'popstate', function() { stripe_handle.close(); });