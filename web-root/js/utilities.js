var Utilities = {
	keyCodeLookupTable : {
		13 : 'enter',
		27 : 'escape',
		16 : 'shift',
		17 : 'control',
		18 : 'alt',
		91 : 'meta',
		20 : 'capslock',
		8 : 'backspace',
		9 : 'tab',
		189 : '-',
		187 : '=',
		219 : '[',
		221 : ']',
		220 : '\\',
		186 : ';',
		222 : '\'',
		188 : ',',
		190 : '.',
		191 : '/'
	},
	
	keyCodeShiftLookupTable : {
		// The ASCII is out of order for the number row symbols so we can't do a translation like we did for numbers and letters
		49 : '!',
		50 : '@',
		51 : '#',
		52 : '$',
		53 : '%',
		54 : '^',
		55 : '&',
		56 : '*',
		57 : '(',
		58 : ')',
		189 : '_',
		187 : '+',
		219 : '{',
		221 : '}',
		220 : '|',
		186 : ':',
		222 : '"',
		188 : '<',
		190 : '>',
		191 : '?'
	},
	
	keyCodeAlphaMin : 65,
	keyCodeAlphaMax : 90,
	
	keyCodeLookup : function( keyEvent ) {
		if ( keyEvent.shiftKey ) { return this.keyCodeShiftLookup( keyEvent ); }
		
		var keyCode = keyEvent.keyCode;
		
		var numericMin = 48;
		var numericMax = 57;
		if ( keyCode <= numericMax && keyCode >= numericMin ) {
			return String.fromCharCode( keyCode );
		}
		
		var alphaOffset = 32;
		if ( keyCode <= this.keyCodeAlphaMax && keyCode >= this.keyCodeAlphaMin ) {
			return String.fromCharCode( keyCode + alphaOffset );
		}
		
		if ( typeof this.keyCodeLookupTable[ keyCode ] === 'undefined' ) {
			return null;
		} else {
			return this.keyCodeLookupTable[ keyCode ];
		}
		
		return null;
	},
	
	keyCodeShiftLookup( keyEvent ) {
		var keyCode = keyEvent.keyCode;
		
		if ( keyCode <= this.keyCodeAlphaMax && keyCode >= this.keyCodeAlphaMin ) {
			return String.fromCharCode( keyCode );
		}
		
		if ( typeof this.keyCodeShiftLookupTable[ keyCode ] === 'undefined' ) {
			if ( typeof this.keyCodeLookupTable[ keyCode ] === 'undefined' ) {
				return null;
			} else {
				return this.keyCodeLookupTable[ keyCode ];
			}
		} else {
			return this.keyCodeShiftLookupTable[ keyCode ];
		}
		
		return null;
	},
	
	isEnter : function( keyEvent ) {
		var keyCode = keyEvent.keyCode;
		if ( typeof this.keyCodeLookupTable[ keyCode ] === 'undefined' ) { return false; }
		return this.keyCodeLookupTable[ keyCode ] === 'enter';
	},
	
	//Not memory efficient
	compareArrays : function( array1, array2 ) {
		return array1.join( '~~' ) === array2.join( '~~' );
	},
	
	//TODO: Is this useful for debugging, or does it obfuscate where the error originated?
	customError : function( errorName, errorMessage ) {
		throw {
			'name' : errorName,
			'message' : errorMessage
		};
	},
	
	sixHexToRgb : function( sixHex ) {
		var rgb = new Array();
		rgb[ 0 ] = this.hexToDecimal( sixHex.substr( 0, 2 ) );
		rgb[ 1 ] = this.hexToDecimal( sixHex.substr( 2, 2 ) );
		rgb[ 2 ] = this.hexToDecimal( sixHex.substr( 4, 2 ) );
		return rgb;
	},
	
	rgbToSixHex : function( rgb ) {
		var sixHex = '';
		sixHex += this.decimalToHex( rgb[ 0 ] );
		sixHex += this.decimalToHex( rgb[ 1 ] );
		sixHex += this.decimalToHex( rgb[ 2 ] );
		return sixHex;
	},
	
	decimalToHex : function( decimal, padLength ) {
		var hex = decimal.toString( 16 );
		if ( ! padLength ) { padLength = 2; }
		while ( hex.length < padLength ) { hex = '0' + hex; }
		return hex;
	},
	
	hexToDecimal : function( hex, padLength ) {
		return parseInt( hex, 16 );
	},
	
	isInteger : function( integerToTest ) {
		return integerToTest === parseInt( integerToTest );
	},
	
	objectEqual : function( object1, object2 ) {
		if ( typeof object1 !== 'object' || typeof object2 !== 'object' ) { return false; }
		
		var objectsAreEqual = true;
		
		for ( var key in object1 ) {
			var value = object1[ key ];
			if ( object2[ key ] === undefined ) {
				objectsAreEqual = false;
				break;
			}
			if ( typeof object2[ key ] === 'object' ) {
				objectsAreEqual = this.objectEqual( object1[ key ], object2[ key ] );
				if ( ! objectsAreEqual ) { break; }
			}
			if ( object1[ key ] !== object2[ key ] ) {
				objectsAreEqual = false;
				break;
			}
		}
		
		return objectsAreEqual;
	},
	
	requestBrowserFullScreen : function() {
		var body = document.body;
		if( body.requestFullScreen ) { body.requestFullScreen(); }	
		else if( body.webkitRequestFullScreen ) { body.webkitRequestFullScreen(); }	
		else if( body.mozRequestFullScreen ) { body.mozRequestFullScreen(); }
	},
	
	formatHumanReadableDollars : function( number ) {
		if ( parseFloat( number ) != number ) { throw "The argument provided was not a number: " + number; }
		var numberString = number.toString();
		if ( ! numberString.match( /\./ ) ) { numberString += '.00'; }
		var dollarsAndCents = numberString.split( '.' );
		if ( dollarsAndCents[ 1 ].length > 2 ) {
			// It's not very obvious what's happening here.
			// We know that we have more digits than we need, so we want to round up:
			// We take the first three digits
			// Convert them to an integer, divide by 10, and take the ceiling
			dollarsAndCents[ 1 ]
				= Math.ceil(
					parseInt(
						dollarsAndCents[ 1 ]
							.substring( 0, 3 )
					) / 10
				).toString();
		}
		
		while ( dollarsAndCents[ 1 ].length < 2 ) { dollarsAndCents[ 1 ] += '0'; }
		
		if ( dollarsAndCents[ 0 ].length > 3 ) {
			var dollars = dollarsAndCents[ 0 ];
			var newDollarsAndCents = '';
			for ( var i = 0; i < dollars.length; i++ ) {
				if ( i != 0 && i % 3 === 0 ) {
					newDollarsAndCents = ',' + newDollarsAndCents;
				}
				newDollarsAndCents
					= dollars[ dollars.length - 1 - i ]
					+ newDollarsAndCents;
			}
			dollarsAndCents[ 0 ] = newDollarsAndCents;
		}
		return '$' + dollarsAndCents[ 0 ] + '.' + dollarsAndCents[ 1 ];
	},
	
	convertDollarsToCents : function( dollars ) {
		if ( parseFloat( dollars ) != dollars ) { throw "The argument provided was not a dollar amount: " + dollars; }
		return Math.ceil( parseFloat( dollars ) * 100 );
	}
};

if ( typeof console === 'undefined' ) { var console = { log : function() {} }; }