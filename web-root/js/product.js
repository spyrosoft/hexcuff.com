$( '.thumbnail-image' ).click( swap_main_product_image );

function swap_main_product_image( click_event )
{
	var swapping_img_url = $( '.main-image' ).attr( 'src' );
	$( '.main-image' ).attr( 'src', $( this ).attr( 'src' ) );
	$( this ).attr( 'src', swapping_img_url );
}

$( '#quantity' ).change( update_total );

function update_total()
{
	var price = parseFloat( $( '#price' ).val() );
	var quantity = parseInt( $( '#quantity' ).val() );
	$( '#total' ).html( price * quantity );
}

update_total();



/* Stripe */

var stripe_handle = StripeCheckout.configure({
	key: 'pk_test_RuyrACKp8FG3rI4f26vgMu02',
	image: '/images/hex-cuff-logo-square.svg',
	locale: 'auto',
	bitcoin: true,
	zipCode: true,
	shippingAddress: true,
	token: stripe_submit_order
});

$( 'button.checkout' ).on( 'click', stripe_checkout );

function stripe_checkout( click_event ) {
	click_event.preventDefault();
	try {
		var price = parseFloat( $( '#price' ).html() );
		var quantity = parseInt( $( '#quantity' ).html() );
		var total_in_cents = Utilities.convertDollarsToCents( price * quantity );
	} catch ( error ) {
		alert( 'Something is wrong with the shopping cart. We have been notified, and will resolve the issue as quickly as possible. Please try again after a while.' );
		notify_admin(
			'The product price was not able to be populated.\n\nPage: '
				+ window.location
				+ '\n\nError: '
				+ error
		);
		return;
	}
	stripe_handle.open({
		'name' : 'Hex Cuff',
		'description' : 'REPLACE ME WITH REAL HEX CUFFS',
		'amount' : total_in_cents
	});
}

function notify_admin( message ) {
	var message_fields_and_values = {
		'message' : message
	};
	$.post( '/notify-admin-ajax/', message_fields_and_values );
}

$( window ).on( 'popstate', function() { stripe_handle.close(); });

/* Our Server */

function stripe_submit_order( stripe_token ) {
	var new_order_fields_and_values = {
		'token' : stripe_token.id
	};
	$.post( '/new-order-ajax/', new_order_fields_and_values )
		.done( stripe_transaction_success )
		.fail( stripe_transaction_fail );
}

function stripe_transaction_success( server_response ) {
	try {
		server_response = JSON.parse( server_response );
	} catch ( error ) {
		stripe_transaction_fail();
		return;
	}
	if ( typeof server_response[ 'success' ] === 'undefined' )
	{
		stripe_transaction_fail();
	}
	else if ( server_response[ 'success' ] === 'true' )
	{
		alert( 'We have received your order and will ship your Hex Cuff within the next two weeks.' );
	}
	else if ( typeof server_response[ 'message' ] !== 'undefined' )
	{
		alert( 'The transaction was rejected. The payment gateway\'s server_response: ' + server_response[ 'message' ] );
	}
	else
	{
		stripe_transaction_fail();
	}
}

function stripe_transaction_fail( server_response ) {
	alert( 'Something went wrong while communicating with the server. It is possible that the payment gateway is down. Please try again after a while. If the problem persists, please contact us and let us know.' );
}