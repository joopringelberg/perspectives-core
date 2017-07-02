"use strict";

exports.pushInDestructiveArray = function( a )
{
	return function( v )
	{
		a.push( v );
		return a;
	};
};

exports.setInDestructiveArrayAt = function( a )
{
	return function( i )
	{
		return function( v )
		{
			a.splice( i, 0, v );
			return a;
		};
	};
};

exports.removeFromDestructiveArrayAt = function( a )
{
	return function( i )
	{
		a.spice( i, 0 );
		return a;
	};
};
