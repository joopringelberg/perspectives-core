/**
 * Created by joopringelberg on 11-07-17.
 */
"use strict";

var location = require( '../src/location' );

function showLoc(l, n)
{
	console.log( "Location " + n + " has value " + location.getLoc( l ) );
}

var l1 = location.locate( 1 );
showLoc( l1, 'l1');

l1.onChange(
	function( v )
	{
		showLoc( l1, 'l1');
	}
);
l1.set( 10 );

var l2 = location.mapLoc(
	function( x )
	{
		return x * x;
	} )( l1 );

showLoc( l2, 'l2');
l2.onChange(
	function( v )
	{
		showLoc( l2, 'l2');
	}
);

function curriedPlus( x )
{
	return function( y )
	{
		return x + y;
	};
}

var l3 = location.mapLoc( curriedPlus )( l1 );
showLoc( l3, 'l3');

l3.onChange(
	function( v )
	{
		showLoc( l3, 'l3');
	}
);

var l4 = location.applyLoc( l3 )( l2 );

showLoc( l4, 'l4');

l4.onChange(
	function( v )
	{
		showLoc( l4, 'l4');
	}
);

location.setLocationValue( l1 )(20)();

location.propagateTheoryDeltas();
