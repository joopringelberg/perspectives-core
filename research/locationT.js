"use strict";

exports.copyToLocation = function( computationOfb ) {
	return function( computationOfloca ) {
		return function() {
      // Compute loca
      var loca = computationOfloca();
			// Compute a result.
			var locx = computationOfb();
			// Save it in a copy-location.
			var locr = exports.saveInLocation( locx.get() );
			// Give the resulting location two supports (make it depend on both the original location loca and the computed result).
			loca.addDependent( "some name", locr );
			locx.addDependent( "another name", locr );
			// Install an update function into the resulting location.
			locr.fun = function() {
				var newLocx;
				if( locr.get() === locx.get() )
				{
					// If content of loca has changed, we recompute b from it.
					newLocx = computationOfb();
					locr.set( newLocx.get() );
					// We replace the original support with the newly computed result.
					locx.removeDependent( "some name" );
					newLocx.setDependent( "some name", newLocx );
				}
				else
				{
					// If the value of locr is not equal to the value of locx, copy locx's value to locr.
					locr.set( locx.get() );
				}
			};
			return locr;
		};
	};
};
