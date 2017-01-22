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

var stripe_key = 'pk_live_MJOzBT97LnvpLYpkXWho4TZt';
if (window.location.hostname === 'localhost') {
    stripe_key = 'pk_test_RuyrACKp8FG3rI4f26vgMu02';
}

var stripe_data = {
	key: stripe_key,
	image: '/images/hex-cuff-logo-square.svg',
	locale: 'auto',
	bitcoin: true,
	zipCode: true,
	billingAddress: true,
	shippingAddress: true,
	token: stripe_submit_order
};

var stripe_handle = StripeCheckout.configure(stripe_data);

$( 'button.checkout' ).on( 'click', stripe_checkout );

function stripe_checkout( click_event ) {
	click_event.preventDefault();
	try {
		var price = parseFloat( $( '#price' ).val() );
		var quantity = parseInt( $( '#quantity' ).val() );
		var total_in_cents = Utilities.convertDollarsToCents( price * quantity );
	} catch ( error ) {
		shopping_cart_failure(
			'The product price was not able to be populated.\n\nPage: '
				+ window.location
				+ '\n\nError: '
				+ error
		);
		return;
	}
	var cuff_description = $( '#which-cuff' ).html() + ', Size: ' + $( '#slot-size' ).val() + ', Ear: ' + $( '#which-ear' ).val() + ', Quantity: ' + $( '#quantity' ).val();
	stripe_handle.open({
		'name' : 'Hex Cuff',
		'description' : cuff_description,
		'amount' : total_in_cents,
		'billingAddress' : true
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

function stripe_submit_order( stripe_token, stripe_additional_data ) {
	var new_order_fields_and_values = {
		'customer-email' : stripe_token.email,
		'which-cuff' : $( '#which-cuff' ).html(),
		'slot-size' : $( '#slot-size' ).val(),
		'which-ear' : $( '#which-ear' ).val(),
		'quantity' : $( '#quantity' ).val(),
		'stripe-token' : stripe_token.id,
		'shipping-name' : stripe_additional_data.shipping_name,
		'shipping-address-line-1' : stripe_additional_data.shipping_address_line1,
		'shipping-address-line-2' : stripe_additional_data.shipping_address_line2,
		'shipping-city' : stripe_additional_data.shipping_address_city,
		'shipping-state' : stripe_additional_data.shipping_address_state,
		'shipping-zip' : stripe_additional_data.shipping_address_zip
	};
	$.post( '/new-order-ajax/', new_order_fields_and_values )
		.done( stripe_transaction_success )
		.fail( stripe_transaction_fail );
}

function stripe_transaction_success( server_response ) {
	try {
		server_response = JSON.parse( server_response );
	} catch ( error ) {
		shopping_cart_failure( 'Could not parse the server response JSON.' );
		return;
	}
	if ( typeof server_response[ 'success' ] === 'undefined' )
	{
		shopping_cart_failure( 'The server response JSON did not provide a success boolean.' );
	}
	else if ( server_response[ 'success' ] === true )
	{
		alert( 'We have received your order and will ship your Hex Cuff within the next two weeks.' );
		window.location = '/';
	}
	else if ( server_response[ 'success' ] === false && typeof server_response[ 'message' ] === 'undefined' )
	{
		shopping_cart_failure( 'The server responded with success as false and no message.' );
	}
	else
	{
		shopping_cart_failure( server_response[ 'message' ] );
	}
}

function stripe_transaction_fail() {
	alert( 'The connection with the server has gone away. Try refreshing the page. Your internet connection may be the cause, or the site may be down.' );
}

function shopping_cart_failure( message ) {
	console.log( message );
	notify_admin( message );
	alert( 'Something is wrong with the shopping cart. We have been notified, and will resolve the issue as quickly as possible. Please try again after a while.' );
}