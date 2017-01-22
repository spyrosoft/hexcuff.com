$('#contact-form').submit( contact_form_submission );
var contact_form_field_names = [ 'name', 'email', 'message' ];

function contact_form_submission( submission_event )
{
	submission_event.preventDefault();
	var contact_fields_and_values = get_contact_fields_and_values_object();
	if ( contact_fields_and_values[ 'message' ] === '' )
	{
		alert( 'The Message field is required.' );
		$( '#contact-form [name=message]' ).focus();
		return;
	}
	$.post( '/contact-us-ajax/', contact_fields_and_values )
		.success( contact_form_submission_complete )
		.fail( contact_form_submission_fail );
}

function get_contact_fields_and_values_object()
{
	var contact_fields_and_values = new Object();
	for ( var i in contact_form_field_names )
	{
		var current_field = contact_form_field_names[ i ];
		try {
			contact_fields_and_values[ current_field ]
				= $( '#contact-form [name=' + current_field + ']' ).val();
		} catch ( error ) {
			//TODO: Convert this to an automated admin notification
			alert( 'One of the form fields has changed and needs to be updated in the code. Some or all of your message failed to reach the server. Please submit another let us know by submitting the form again with this message, and we will sort out the issue.' );
		}
	}
	return contact_fields_and_values;
}

function contact_form_submission_complete( response_data ) {
	try {
		var response_json = JSON.parse( response_data );
		if ( response_json[ 'success' ] ) {
			alert( 'Thank you for contacting me.' );
			clear_contact_form();
		} else {
			if ( response_json[ 'error' ] ) {
				alert( 'Something went wrong. Your message has not been sent. The server replied: ' + response_json[ 'error' ] );
			} else {
				//TODO: Convert this to an automated admin notification
				alert( 'Something went wrong. Please contact us by some other means to let us know our contact form is not working.' );
			}
		}
	} catch ( error ) {
		//TODO: Convert this to an automated admin notification
		alert( 'The server responded with invalid data. Please contact us by some other means to let us know our contact form is not working.' );
	}
}

function contact_form_submission_fail() {
	alert( 'Connection with the server failed. Please check your internet connection. Otherwise, something is wrong on our end - please try again after a while.' );
}

function clear_contact_form()
{
	document.getElementById( 'contact-form' ).reset();
}