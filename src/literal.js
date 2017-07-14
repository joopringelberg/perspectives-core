"use strict";

exports.literal = function literal( valueString )
{
	return valueString;
};

function parse( value )
{
	if( isISODateString( value ) )
	{
		return new Date( value );
	}
	else if( typeof value === 'function' )
	{
		return value;
	}
	else
	{
		return JSON.parse( value );
	}
}

var ISORegexp = new RegExp( /\d{4}-[01]\d-[0-3]\dT[0-2]\d:[0-5]\d:[0-5]\d\.\d+([+-][0-2]\d:[0-5]\d|Z)/ );

function isISODateString( valueString )
{
	return ISORegexp.test( valueString );
}

function toString( value )
{
	if( value instanceof Date )
	{
		return value.toISOString();
	}
	else if( typeof value === 'function' )
	{
		return value;
	}
	else
	{
		return JSON.stringify( value );
	}
}

exports.showLit = function( value )
{
	if ( typeof value === 'function' )
	{
		return value.toString();
	}
	else
	{
		return value;
	}
};

exports.mapLit = function mapLit( fun )
{
	return function( valueString )
	{
		return toString( fun( parse( valueString ) ) );
	};
};

exports.applyLit = function( funLit )
{
	return function( valueLit )
	{
		return toString( funLit( parse( valueLit ) ) );
	};
};