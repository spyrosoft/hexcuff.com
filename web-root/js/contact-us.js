$('#contact-form').submit( contact_form_submission );
var contact_form_field_names = [ 'name', 'subject', 'email', 'message' ];

function contact_form_submission( submission_event )
{
	submission_event.preventDefault();
	var contact_fields_and_values = new Object;
	for ( var i in contact_form_field_names )
	{
		var current_field = contact_form_field_names[ i ];
		try {
			contact_fields_and_values[ current_field ]
				= $( '.contact form [name=' + current_field + ']' ).val();
		} catch ( error ) {
			alert( 'One of the form fields has changed and needs to be updated in the code. Some or all of your message was lost. Please submit another let us know by submitting the form again with this message, and we will sort out the issue.' );
		}
	}
	if ( contact_fields_and_values[ 'message' ] === '' )
	{
		alert( 'The Message field is required.' );
		$( '.contact form [name=message]' ).focus();
		return;
	}
	$.post( '/contact-ajax/', contact_fields_and_values )
		.done( clear_contact_form )
		.success(
			function( response_data ) {
				try {
					var response_json = JSON.parse( response_data );
					if ( response_json[ 'success' ] ) {
						alert( 'We got your message. Thank you for your contact.'  );
					} else {
						alert( 'Something went wrong. Please contact us by some other means to let us know our contact form is not working.' );
					}
				} catch ( error ) { alert( 'The server responded with invalid data. Please contact us by some other means to let us know our contact form is not working.' ); }
			}
		)
		.fail(
			function() {
				alert( 'Connection with the server failed. Please check your internet connection. Otherwise, something is wrong on our end - please try again later.' );
			}
		);
}

function clear_contact_form()
{
	document.getElementById( 'contact-form' ).reset();
}