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